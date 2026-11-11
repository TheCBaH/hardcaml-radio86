(* The drivers and the fixtures: for each workload a small 8080 program of our own that reads its
   cases from RAM, calls the ported routine and writes the answers to RAM, and the expected RAM
   computed by {!Oracle}. Authored in this repository (MIT); the routines it calls are the ports
   in ackermann.ml, mul16.ml and div32x16r.ml, whose licences are their own.

   Every driver ends with [sp_save] <- SP, then HLT, so the final stack pointer is visible in RAM
   to any CPU model. *)

open Fixture

let bytes_of_string s = List.init (String.length s) (fun i -> Char.code s.[i])

(* The data image: guard, inputs, results, scratch variables (with initial values), guard. *)
let data_image ~table ~results_len ~vars =
  let open I8080_asm.Items in
  let items =
    [
      db (bytes_of_string guard);
      label "table";
      db (bytes_of_string table);
      label "results";
      space results_len;
      label "vars";
    ]
    @ List.concat_map (fun (name, init) -> [ label name; db (bytes_of_string init) ]) vars
    @ [ label "vars_end"; db (bytes_of_string guard) ]
  in
  match I8080_asm.Program.assemble ~origin:data_origin items with
  | Ok img -> img
  | Error e -> failwith (Foundation.Diagnostic.render_all (Foundation.Diag.diagnostics e))

let assemble_code items =
  match I8080_asm.Program.assemble ~origin:0 items with
  | Ok img -> img
  | Error e -> failwith (Foundation.Diagnostic.render_all (Foundation.Diag.diagnostics e))

let build ~name ~sources ~driver ~routines ~table ~results ~vars ~budget =
  let results_bytes = String.concat "" results in
  let vars = vars @ [ ("sp_save", words [ 0 ]) ] in
  let data = data_image ~table ~results_len:(String.length results_bytes) ~vars in
  let at = symbol_exn data in
  let code = assemble_code (driver at @ routines) in
  let data_end = at "vars_end" in
  {
    name;
    sources;
    code;
    data;
    extra = [ stack_guard ];
    writable =
      [
        { name = "results"; lo = at "results"; len = String.length results_bytes };
        { name = "scratch"; lo = at "vars"; len = data_end - at "vars" };
        stack_region;
      ];
    expect = [ (at "results", results_bytes); (at "sp_save", words [ stack_top ]) ];
    final_sp = stack_top;
    budget;
  }

let finish at =
  let open I8080_asm.Items in
  [ lxi hl 0; dad sp; shld (at "sp_save"); hlt () ]

(* {1 Ackermann}

   The table holds (M, N) words ended by a word with high byte 0xff; the driver loads DE = M,
   HL = N, calls ack and stores HL. BC walks the table, which ack leaves alone. *)

let ack_driver at =
  let open I8080_asm.Items in
  [
    lxi sp stack_top;
    lxi hl (at "results");
    shld (at "rptr");
    lxi bc (at "table");
    label "next";
    inx bc;
    ldax bc;
    dcx bc;
    cpi 0xff;
    jz_e (symbol "done");
    ldax bc;
    mov e a;
    inx bc;
    ldax bc;
    mov d a;
    inx bc;
    ldax bc;
    mov l a;
    inx bc;
    ldax bc;
    mov h a;
    inx bc;
    call_e (symbol "ack");
    xchg ();
    lhld (at "rptr");
    mov m e;
    inx hl;
    mov m d;
    inx hl;
    shld (at "rptr");
    jmp_e (symbol "next");
    label "done";
  ]
  @ finish at

let ackermann ?(budget = 50_000) cases =
  build ~name:"ackermann"
    ~sources:[ "rosettacode-ackermann: ackermann.ml" ]
    ~driver:ack_driver ~routines:Ackermann.routine
    ~table:(String.concat "" (List.map (fun (m, n) -> words [ m; n ]) cases) ^ words [ 0xffff; 0 ])
    ~results:(List.map (fun (m, n) -> words [ Oracle.ackermann m n ]) cases)
    ~vars:[ ("rptr", words [ 0 ]) ]
    ~budget

(* The cases of the proposal's table, plus the two with N = 0 that take the other branch. *)
let ackermann_cases = [ (0, 0); (0, 7); (1, 0); (1, 4); (2, 0); (2, 4); (3, 2) ]

(* {1 Multiply and divide} *)

let mul_driver at =
  let open I8080_asm.Items in
  [
    lxi sp stack_top;
    lxi hl (at "table");
    shld (at "iptr");
    lxi hl (at "results");
    shld (at "rptr");
    label "loop";
    lda (at "cnt");
    ora a;
    jz_e (symbol "done");
    dcr a;
    sta (at "cnt");
    lhld (at "iptr");
    mov e m;
    inx hl;
    mov d m;
    inx hl;
    mov c m;
    inx hl;
    mov b m;
    inx hl;
    shld (at "iptr");
    xchg ();
    call_e (symbol "mul16");
    lhld (at "rptr");
    mov m e;
    inx hl;
    mov m d;
    inx hl;
    shld (at "rptr");
    jmp_e (symbol "loop");
    label "done";
  ]
  @ finish at

let multiply ?(budget = 16_000) cases =
  build ~name:"multiply" ~sources:[ "litwr2-pi-spigot: mul16.ml" ] ~driver:mul_driver
    ~routines:Mul16.routine
    ~table:(String.concat "" (List.map (fun (x, y) -> words [ x; y ]) cases))
    ~results:(List.map (fun (x, y) -> words [ Oracle.mul16 x y ]) cases)
    ~vars:
      [
        ("iptr", words [ 0 ]);
        ("rptr", words [ 0 ]);
        ("cnt", String.make 1 (Char.chr (List.length cases)));
      ]
    ~budget

let multiply_cases =
  [
    (0, 0);
    (0, 5);
    (1, 1);
    (1, 0xffff);
    (0xffff, 1);
    (0xffff, 0xffff);
    (0x5555, 3);
    (0xaaaa, 0x5555);
    (0x7fff, 2);
    (0x8000, 2);
    (0x8000, 0x8000);
    (0x8001, 0x8001);
    (0x1234, 0x5678);
    (0xff, 0xff);
    (0x100, 0x100);
  ]

(* A case is (hi, lo, divisor) with hi < divisor; the table order of a case is divisor, lo, hi. *)
let div_driver at =
  let open I8080_asm.Items in
  [
    lxi sp stack_top;
    lxi hl (at "table");
    shld (at "iptr");
    lxi hl (at "results");
    shld (at "rptr");
    label "loop";
    lda (at "cnt");
    ora a;
    jz_e (symbol "done");
    dcr a;
    sta (at "cnt");
    lhld (at "iptr");
    lxi de 6;
    push hl;
    dad de;
    shld (at "iptr");
    pop hl;
    xchg ();
    ldax de;
    mov c a;
    inx de;
    ldax de;
    mov b a;
    inx de;
    ldax de;
    mov l a;
    inx de;
    ldax de;
    mov h a;
    inx de;
    xchg ();
    mov a m;
    inx hl;
    mov h m;
    mov l a;
    call_e (symbol "div32x16r");
    push hl;
    lhld (at "rptr");
    mov m e;
    inx hl;
    mov m d;
    inx hl;
    pop de;
    mov m e;
    inx hl;
    mov m d;
    inx hl;
    shld (at "rptr");
    jmp_e (symbol "loop");
    label "done";
  ]
  @ finish at

let divide ?(budget = 42_000) cases =
  build ~name:"divide"
    ~sources:[ "litwr2-pi-spigot: div32x16r.ml" ]
    ~driver:div_driver ~routines:Div32x16r.routine
    ~table:(String.concat "" (List.map (fun (hi, lo, d) -> words [ d; lo; hi ]) cases))
    ~results:
      (List.map
         (fun (hi, lo, d) ->
           let q, r = Oracle.div32x16 ~hi ~lo ~d in
           words [ q; r ])
         cases)
    ~vars:
      [
        ("iptr", words [ 0 ]);
        ("rptr", words [ 0 ]);
        ("cnt", String.make 1 (Char.chr (List.length cases)));
      ]
    ~budget

let divide_cases =
  [
    (0, 0, 1);
    (0, 0, 0xffff);
    (0, 1, 1);
    (0, 0xffff, 1);
    (0, 100, 7);
    (0, 60000, 100);
    (0, 0xffff, 0xffff);
    (0, 0xfffe, 0xffff);
    (1, 0, 2);
    (2, 0xfffd, 3);
    (2, 0xffff, 3);
    (0x7fff, 0xffff, 0x8000);
    (0x8000, 0, 0x8001);
    (0x8000, 0x1234, 0x9000);
    (0xfffe, 0xffff, 0xffff);
    (0x1234, 0x5678, 0x9abc);
  ]

(* {1 Pi spigot}

   The ported spigot is entered at pi_start with HL = the number of digits and writes its characters
   through putc, which this driver defines (a store through [bufptr]); it ends at showtimer, which
   stores SP and halts. The program is laid out as the source's: the three pages of m10000 on a page
   boundary and the array r (label ra) right after them. The routine patches its own operands, so
   the words it patches (svde, m1, svhl, mz1 .. mz3) and its variables cv and kv are writable code. *)

let fresh_gen () =
  let n = ref 0 in
  fun base ->
    incr n;
    Printf.sprintf "%s_%d" base !n

let spigot ?(digits = 32) ?(budget = 1_000_000) () =
  if digits < 4 || digits mod 4 <> 0 then
    invalid_arg "Fixtures.spigot: digits must be a multiple of 4";
  let n = 7 * digits / 2 in
  let text = Oracle.pi_digits digits in
  let data =
    data_image ~table:(words [ digits ]) ~results_len:digits
      ~vars:[ ("bufptr", words [ 0 ]); ("sp_save", words [ 0 ]) ]
  in
  let at = symbol_exn data in
  let driver =
    let open I8080_asm.Items in
    [
      lxi sp stack_top;
      lxi hl (at "results");
      shld (at "bufptr");
      lhld (at "table");
      jmp_e (symbol "pi_start");
      label "showtimer";
      lxi hl 0;
      dad sp;
      shld (at "sp_save");
      hlt ();
      label "putc";
      lhld (at "bufptr");
      mov m e;
      inx hl;
      shld (at "bufptr");
      ret ();
    ]
  in
  let code pad =
    assemble_code
      (driver
      @ Pi_spigot.routine ~fresh:(fresh_gen ())
      @ Div32x16r.routine
      @ [ I8080_asm.space pad ]
      @ Pi_mul10000.routine
      @ [ I8080_asm.label "ra"; I8080_asm.space ((2 * n) + 2) ])
  in
  let pad = (256 - (symbol_exn (code 0) "m10000" mod 256)) mod 256 in
  let code = code pad in
  let cs = symbol_exn code in
  let region name lo len = { name; lo; len } in
  {
    name = Printf.sprintf "spigot%d" digits;
    sources = [ "litwr2-pi-spigot: pi_spigot.ml pi_div.ml pi_div8.ml pi_mul10000.ml div32x16r.ml" ];
    code;
    data;
    extra = [ stack_guard ];
    writable =
      [
        region "results" (at "results") digits;
        region "scratch" (at "vars") (at "vars_end" - at "vars");
        stack_region;
        region "r" (cs "ra" + 2) (2 * n);
        region "cv" (cs "cv") 2;
        region "kv" (cs "kv") 2;
        region "svhl" (cs "svhl") 2;
        region "svde operand" (cs "svde" + 1) 2;
        region "m1 operand" (cs "m1" + 1) 2;
        region "mz1 operand" (cs "mz1" + 1) 1;
        region "mz2 operand" (cs "mz2" + 1) 2;
        region "mz3 operand" (cs "mz3" + 1) 1;
      ];
    expect = [ (at "results", text); (at "sp_save", words [ stack_top ]) ];
    final_sp = stack_top;
    budget;
  }

let all () =
  [
    ackermann ackermann_cases;
    multiply multiply_cases;
    divide divide_cases;
    spigot ~budget:900_000 ();
  ]
