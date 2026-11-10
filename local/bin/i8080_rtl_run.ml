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

let run image hex origin stim tick_every vcd pin_trace max_tstates =
  let memory = Harness.new_memory () in
  Harness.load_program memory ~origin (if hex then hex_image (read_file image) else read_file image);
  let stim =
    match stim with
    | None -> die "a stimulus is needed: reset is held until the schedule releases it"
    | Some f -> ( match Stimulus.parse (read_file f) with Ok s -> s | Error e -> die "%s: %s" f e)
  in
  let oc = Option.map open_out_bin vcd in
  let r = Harness.run ~tick_every ~max_tstates ?vcd:oc ~stim ~memory () in
  Option.iter close_out oc;
  Option.iter
    (fun f -> Out_channel.with_open_bin f (fun oc -> output_string oc (Harness.pin_trace r)))
    pin_trace;
  Printf.printf "tstates %s\n"
    (match r.counted with Some c -> string_of_int c | None -> "none (the run did not halt)");
  List.iter
    (fun (w : Harness.write) -> Printf.printf "write %02x@%04x at sample %d\n" w.data w.addr w.at)
    r.writes;
  match r.trapped with
  | Some (n, cause) ->
      Printf.printf "trap at sample %d, cause %s\n" n
        (if cause = Core.cause_opcode then "opcode not implemented" else "HOLD or INT requested");
      exit 1
  | None -> ()

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
      $ Arg.(value & opt int 100_000 & info [ "max-tstates" ] ~docv:"N"))

let verilog output =
  let module Circ = Hardcaml.Circuit.With_interface (Core.I) (Core.O) in
  let c = Circ.create_exn ~name:"i8080_core" Core.create in
  match output with
  | None -> Hardcaml.Rtl.print Verilog c
  | Some f ->
      let oc = open_out_bin f in
      Hardcaml.Rtl.output ~output_mode:(To_channel oc) Verilog c;
      close_out oc

let verilog_cmd =
  Cmd.v
    (Cmd.info "verilog" ~doc:"Write the core as Verilog.")
    Term.(const verilog $ Arg.(value & opt (some string) None & info [ "output" ] ~docv:"FILE"))

let () = exit (Cmd.eval (Cmd.group (Cmd.info "i8080_rtl_run") [ run_cmd; verilog_cmd ]))
