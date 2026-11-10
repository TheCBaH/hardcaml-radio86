(* The MVI/STA/HLT fixture (K3/K4): 3e 2a 32 00 20 76 at 0, RAM everywhere. *)
let program = "\x3e\x2a\x32\x00\x20\x76"
let nowait = "# i8080-stimulus v1\nreset 0 @4\n"
let wait2 = "# i8080-stimulus v1\nreset 0 @4\nready 0 @m5+1\nready 1 @m5+3\n"

let run ?tick_every ?vcd stim =
  let memory = I8080_rtl.Harness.new_memory () in
  I8080_rtl.Harness.load_program memory ~origin:0 program;
  let stim = match I8080_rtl.Stimulus.parse stim with Ok s -> s | Error e -> failwith e in
  I8080_rtl.Harness.run ?tick_every ?vcd ~stim ~memory ()
