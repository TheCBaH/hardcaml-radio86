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
  let open I8080_asm in
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
  match Program.assemble ~origin:data_origin items with
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
  let open I8080_asm in
  [ insn (lxi hl 0); insn (dad sp); insn (shld (at "sp_save")); insn (hlt ()) ]

(* {1 Ackermann}

   The table holds (M, N) words ended by a word with high byte 0xff; the driver loads DE = M,
   HL = N, calls ack and stores HL. BC walks the table, which ack leaves alone. *)

let ack_driver at =
  let open I8080_asm in
  [
    insn (lxi sp stack_top);
    insn (lxi hl (at "results"));
    insn (shld (at "rptr"));
    insn (lxi bc (at "table"));
    label "next";
    insn (inx bc);
    insn (ldax bc);
    insn (dcx bc);
    insn (cpi 0xff);
    insn (jz_e (symbol "done"));
    insn (ldax bc);
    insn (mov e a);
    insn (inx bc);
    insn (ldax bc);
    insn (mov d a);
    insn (inx bc);
    insn (ldax bc);
    insn (mov l a);
    insn (inx bc);
    insn (ldax bc);
    insn (mov h a);
    insn (inx bc);
    insn (call_e (symbol "ack"));
    insn (xchg ());
    insn (lhld (at "rptr"));
    insn (mov m e);
    insn (inx hl);
    insn (mov m d);
    insn (inx hl);
    insn (shld (at "rptr"));
    insn (jmp_e (symbol "next"));
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
  let open I8080_asm in
  [
    insn (lxi sp stack_top);
    insn (lxi hl (at "table"));
    insn (shld (at "iptr"));
    insn (lxi hl (at "results"));
    insn (shld (at "rptr"));
    label "loop";
    insn (lda (at "cnt"));
    insn (ora a);
    insn (jz_e (symbol "done"));
    insn (dcr a);
    insn (sta (at "cnt"));
    insn (lhld (at "iptr"));
    insn (mov e m);
    insn (inx hl);
    insn (mov d m);
    insn (inx hl);
    insn (mov c m);
    insn (inx hl);
    insn (mov b m);
    insn (inx hl);
    insn (shld (at "iptr"));
    insn (xchg ());
    insn (call_e (symbol "mul16"));
    insn (lhld (at "rptr"));
    insn (mov m e);
    insn (inx hl);
    insn (mov m d);
    insn (inx hl);
    insn (shld (at "rptr"));
    insn (jmp_e (symbol "loop"));
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
  let open I8080_asm in
  [
    insn (lxi sp stack_top);
    insn (lxi hl (at "table"));
    insn (shld (at "iptr"));
    insn (lxi hl (at "results"));
    insn (shld (at "rptr"));
    label "loop";
    insn (lda (at "cnt"));
    insn (ora a);
    insn (jz_e (symbol "done"));
    insn (dcr a);
    insn (sta (at "cnt"));
    insn (lhld (at "iptr"));
    insn (lxi de 6);
    insn (push hl);
    insn (dad de);
    insn (shld (at "iptr"));
    insn (pop hl);
    insn (xchg ());
    insn (ldax de);
    insn (mov c a);
    insn (inx de);
    insn (ldax de);
    insn (mov b a);
    insn (inx de);
    insn (ldax de);
    insn (mov l a);
    insn (inx de);
    insn (ldax de);
    insn (mov h a);
    insn (inx de);
    insn (xchg ());
    insn (mov a m);
    insn (inx hl);
    insn (mov h m);
    insn (mov l a);
    insn (call_e (symbol "div32x16r"));
    insn (push hl);
    insn (lhld (at "rptr"));
    insn (mov m e);
    insn (inx hl);
    insn (mov m d);
    insn (inx hl);
    insn (pop de);
    insn (mov m e);
    insn (inx hl);
    insn (mov m d);
    insn (inx hl);
    insn (shld (at "rptr"));
    insn (jmp_e (symbol "loop"));
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

let all () = [ ackermann ackermann_cases; multiply multiply_cases; divide divide_cases ]
