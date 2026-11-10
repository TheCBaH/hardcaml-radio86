(* Every opcode, run on the Cyclesim model of the core and replayed instruction by instruction in Rivet's
   behavioural model (registers, memory accesses, T-states), from three starting states that make
   conditions both true and false. The program sets the registers up with the instructions that the
   comparison also covers. *)
open I8080_trace
open I8080_rtl

let manifest () = match Manifest.load "../manifest.json" with Ok m -> m | Error e -> failwith e

(* 0000: JMP 0100; the rest of page 0 is HLT, so every jump, call and RST target stops the run *)
let low_memory = "\xc3\x00\x01" ^ String.make 0x3d '\x76'
let b l = String.concat "" (List.map (fun x -> String.make 1 (Char.chr x)) l)

let program ~variant op =
  let hl = if op = 0xe9 then 0x0010 else 0x9000 in
  let flags =
    match variant with
    | 0 -> [ 0xaf; 0x3e; 0x9c ] (* XRA A: Z P, then A = 9c *)
    | 1 -> [ 0x3e; 0x81; 0xb7; 0x37; 0x3e; 0x2f ] (* S P C, A = 2f *)
    | _ -> [ 0x3e; 0x01; 0xb7; 0x3e; 0xf0 ]
    (* nothing set, A = f0 *)
  in
  let setup =
    [ 0x31; 0x00; 0x80 (* LXI SP,8000 *); 0x21; 0x10; 0x00 (* LXI H,0010 *); 0xe5 (* PUSH H *) ]
    @ [ 0x01; 0x34; 0x12; 0x11; 0x78; 0x56; 0x21; hl land 0xff; hl lsr 8 ]
    @ flags
  in
  let operands =
    match Plan.length op with
    | 1 -> []
    | 2 -> [ 0x5a ]
    | _ -> if op lsr 6 = 3 then [ 0x10; 0x00 ] else [ 0x00; 0x91 ]
  in
  let operands = if op = 0xdb || op = 0xd3 then [ 0x20 ] else operands in
  b setup ^ b (op :: operands) ^ "\x76"

let run_variant ~variant op =
  let image = program ~variant op in
  let memory = Harness.new_memory () in
  Harness.load_program memory ~origin:0 low_memory;
  Harness.load_program memory ~origin:0x100 image;
  let model = Bytes.to_string (Bytes.copy memory) in
  let stim = match Stimulus.parse "reset 0 @4\n" with Ok s -> s | Error e -> failwith e in
  let file = Filename.temp_file "isa" ".vcd" in
  let oc = open_out_bin file in
  let r = Harness.run ~vcd:oc ~max_tstates:900 ~stim ~memory () in
  close_out oc;
  let m = manifest () in
  let res =
    match Observe.run ~frames:true m (Hwfrp_native.open_file file) with
    | Ok o -> o
    | Error l -> failwith (String.concat "\n" l)
  in
  Sys.remove file;
  let times = Hashtbl.create 64 in
  List.iter (fun (f : Frame.t) -> Hashtbl.replace times f.n f.time) res.frames;
  let setup =
    {
      Lockstep.memory = [ (0, model) ];
      pc = 0;
      sp = 0;
      known =
        [ (0xe3, "access order", "XTHL writes SP+1 before SP; Rivet's model the other way round") ];
      start_regs = None;
    }
  in
  (r, Lockstep.run setup ~time_of:(Hashtbl.find_opt times) (Events.instructions res.events))

let%expect_test "all 256 opcodes agree with Rivet's model in three starting states" =
  let bad = ref [] and runs = ref 0 and instrs = ref 0 in
  for variant = 0 to 2 do
    for op = 0 to 255 do
      incr runs;
      let r, rep = run_variant ~variant op in
      instrs := !instrs + rep.compared;
      match rep.divergence with
      | None -> ()
      | Some d when d.what = "capture" && r.stop = Harness.Budget ->
          (* RST 0 runs the program again until the budget cuts an instruction short *)
          ()
      | Some d ->
          bad :=
            Printf.sprintf "variant %d op %02x: %s" variant op
              (Format.asprintf "%a" Lockstep.pp_divergence d)
            :: !bad
    done
  done;
  List.iter print_endline (List.rev !bad);
  Printf.printf "%d runs, %d divergences\n" !runs (List.length !bad);
  [%expect {| 768 runs, 0 divergences |}]
