open Cmdliner
open I8080_rtl

let read_file f = In_channel.with_open_bin f In_channel.input_all

let die fmt =
  Printf.ksprintf
    (fun s ->
      prerr_endline s;
      exit 2)
    fmt

(* A hex image is one byte per whitespace-separated token, as $readmemh reads it. *)
let hex_image text =
  let toks =
    List.filter (( <> ) "")
      (String.split_on_char '\n' text |> List.concat_map (String.split_on_char ' '))
  in
  String.concat ""
    (List.map
       (fun t ->
         match int_of_string_opt ("0x" ^ String.trim t) with
         | Some v when v >= 0 && v < 256 -> String.make 1 (Char.chr v)
         | _ -> die "bad hex byte %S" t)
       toks)

(* [--vectors]: every clock cycle of the run as a line for the Verilog testbench *)
let with_vectors file f =
  match file with
  | None -> f None
  | Some file ->
      Out_channel.with_open_bin file (fun oc ->
          f (Some (fun i o -> output_string oc (Vectors.line i o ^ "\n"))))

let vectors_arg =
  Arg.(
    value
    & opt (some string) None
    & info [ "vectors" ] ~docv:"FILE" ~doc:"Inputs and expected outputs of every clock cycle.")

let stop_name = function
  | Harness.Halt -> "hlt"
  | Port p -> Printf.sprintf "port %02x" p
  | Budget -> "budget"

let write_io_out file (r : Harness.result) =
  Option.iter
    (fun f ->
      Out_channel.with_open_bin f (fun oc ->
          List.iter (fun (p, b) -> Printf.fprintf oc "%02x %02x\n" p b) r.io_out))
    file

let run image hex origin stim tick_every vcd pin_trace max_tstates vectors boot boot_pc stop_ports
    no_halt_stop in_default io_out =
  let memory = Harness.new_memory () in
  Harness.load_program memory ~origin (if hex then hex_image (read_file image) else read_file image);
  let stim =
    match stim with
    | None -> die "a stimulus is needed: reset is held until the schedule releases it"
    | Some f -> ( match Stimulus.parse (read_file f) with Ok s -> s | Error e -> die "%s: %s" f e)
  in
  let boot = Option.map (fun f -> { Harness.code = hex_image (read_file f); pc = boot_pc }) boot in
  let oc = Option.map open_out_bin vcd in
  let r =
    with_vectors vectors (fun probe ->
        Harness.run ~tick_every ~max_tstates ~record:(pin_trace <> None) ?vcd:oc ?probe ~stim
          ~in_default ~stop_ports ?boot ~halt_stop:(not no_halt_stop) ~memory ())
  in
  Option.iter close_out oc;
  Option.iter
    (fun f -> Out_channel.with_open_bin f (fun oc -> output_string oc (Harness.pin_trace r)))
    pin_trace;
  write_io_out io_out r;
  Printf.printf "tstates %s\n"
    (match r.counted with Some c -> string_of_int c | None -> "none (the run did not halt)");
  Printf.printf "stop %s\n" (stop_name r.stop);
  List.iter
    (fun (w : Harness.write) -> Printf.printf "write %02x@%04x at sample %d\n" w.data w.addr w.at)
    r.writes

let in_default_arg =
  Arg.(
    value & opt int 0xff
    & info [ "in-default" ] ~docv:"BYTE"
        ~doc:"What an IN reads from a port with no bytes left, and an INTA cycle with none.")

let io_out_arg =
  Arg.(
    value
    & opt (some string) None
    & info [ "io-out" ] ~docv:"FILE" ~doc:{|One "port value" hex line per OUT.|})

let run_cmd =
  Cmd.v
    (Cmd.info "run" ~doc:"Run a program on the Cyclesim model of the core.")
    Term.(
      const run
      $ Arg.(
          required & opt (some file) None & info [ "image" ] ~docv:"FILE" ~doc:"Raw program bytes.")
      $ Arg.(value & flag & info [ "hex" ] ~doc:"The image is hex text, one byte per token.")
      $ Arg.(value & opt int 0 & info [ "origin" ] ~docv:"ADDR")
      $ Arg.(
          value & opt (some file) None & info [ "stim" ] ~docv:"FILE" ~doc:"K4 stimulus schedule.")
      $ Arg.(
          value & opt int 1
          & info [ "tick-every" ] ~docv:"N" ~doc:"Implementation clock cycles per T-state.")
      $ Arg.(value & opt (some string) None & info [ "vcd" ] ~docv:"FILE" ~doc:"Vcd.wrap waveform.")
      $ Arg.(
          value & opt (some string) None & info [ "pin-trace" ] ~docv:"FILE" ~doc:"K4 pin trace.")
      $ Arg.(value & opt int 100_000 & info [ "max-tstates" ] ~docv:"N")
      $ vectors_arg
      $ Arg.(
          value
          & opt (some file) None
          & info [ "boot" ] ~docv:"FILE"
              ~doc:"Hex preamble run from 0000 until the first fetch at --boot-pc.")
      $ Arg.(value & opt int 0x100 & info [ "boot-pc" ] ~docv:"ADDR")
      $ Arg.(
          value & opt_all int []
          & info [ "stop-port" ] ~docv:"PORT" ~doc:"Stop after an OUT to this port.")
      $ Arg.(value & flag & info [ "no-halt-stop" ] ~doc:"Keep running in the halt cycle.")
      $ in_default_arg $ io_out_arg)

let default_stim = {|# i8080-stimulus v1
reset 0 @4
|}

(* A CP/M .COM file under the K5 shim: loaded at 0100 behind the release's boot preamble, run until
   OUT FF or HLT. The console is port 01. *)
let com file hex stim in_default tick_every vcd pin_trace max_tstates io_out vectors =
  let memory = Harness.new_memory () in
  Harness.load_shim memory;
  Harness.load_program memory ~origin:0x100
    (if hex then hex_image (read_file file) else read_file file);
  let stim =
    match Stimulus.parse (match stim with Some f -> read_file f | None -> default_stim) with
    | Ok s -> s
    | Error e -> die "stimulus: %s" e
  in
  let oc = Option.map open_out_bin vcd in
  let r =
    with_vectors vectors (fun probe ->
        Harness.run ~tick_every ~max_tstates ~record:(pin_trace <> None) ?vcd:oc ?probe ~stim
          ~in_default ~stop_ports:[ 0xff ] ~boot:Harness.com_boot ~memory ())
  in
  Option.iter close_out oc;
  Option.iter
    (fun f -> Out_channel.with_open_bin f (fun oc -> output_string oc (Harness.pin_trace r)))
    pin_trace;
  write_io_out io_out r;
  let console = Buffer.create 64 in
  List.iter (fun (p, b) -> if p = 1 then Buffer.add_char console (Char.chr b)) r.io_out;
  print_string (Buffer.contents console);
  Printf.printf "stop %s, tstates %s, %d writes, %d OUTs\n" (stop_name r.stop)
    (match r.counted with Some c -> string_of_int c | None -> "none")
    (List.length r.writes) (List.length r.io_out);
  match r.stop with Budget -> exit 1 | _ -> ()

let com_cmd =
  Cmd.v
    (Cmd.info "com" ~doc:"Run a CP/M .COM file under the K5 shim on the Cyclesim model of the core.")
    Term.(
      const com
      $ Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE.COM")
      $ Arg.(value & flag & info [ "hex" ] ~doc:"The file is hex text, one byte per token.")
      $ Arg.(
          value & opt (some file) None & info [ "stim" ] ~docv:"FILE" ~doc:"K4 stimulus schedule.")
      $ in_default_arg
      $ Arg.(
          value & opt int 1
          & info [ "tick-every" ] ~docv:"N" ~doc:"Implementation clock cycles per T-state.")
      $ Arg.(value & opt (some string) None & info [ "vcd" ] ~docv:"FILE" ~doc:"Vcd.wrap waveform.")
      $ Arg.(
          value & opt (some string) None & info [ "pin-trace" ] ~docv:"FILE" ~doc:"K4 pin trace.")
      $ Arg.(value & opt int 10_000_000 & info [ "max-tstates" ] ~docv:"N")
      $ io_out_arg $ vectors_arg)

let verilog output testbench bench =
  let module Circ = Hardcaml.Circuit.With_interface (Core.I) (Core.O) in
  let c = Circ.create_exn ~name:"i8080_core" Core.create in
  (match output with
  | None -> Hardcaml.Rtl.print Verilog c
  | Some f ->
      let oc = open_out_bin f in
      Hardcaml.Rtl.output ~output_mode:(To_channel oc) Verilog c;
      close_out oc);
  let write text =
    Option.iter (fun f -> Out_channel.with_open_bin f (fun oc -> output_string oc text))
  in
  write Vectors.testbench testbench;
  write Bench.verilog bench

let verilog_cmd =
  Cmd.v
    (Cmd.info "verilog" ~doc:"Write the core as Verilog.")
    Term.(
      const verilog
      $ Arg.(value & opt (some string) None & info [ "output" ] ~docv:"FILE")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "testbench" ] ~docv:"FILE"
              ~doc:"A testbench that replays the lines of --vectors into the core.")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "bench" ] ~docv:"FILE"
              ~doc:
                "A bench that runs a CP/M .COM under the K5 shim (+com=FILE), for Verilator or \
                 Icarus Verilog."))

let () = exit (Cmd.eval (Cmd.group (Cmd.info "i8080_rtl_run") [ run_cmd; com_cmd; verilog_cmd ]))
