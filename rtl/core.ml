(* i8080 datapath and sequencer skeleton (P2): reset, fetch, MVI A, STA, HLT and READY waits.

   One enabled clock cycle is one T-state (K2). The pins are a function of the state registers only
   (Moore), so the values seen in the cycle that ends T-state k are the pins of T-state k together with
   the inputs applied during it: this is the sample of the K4 pin trace. Every other opcode is
   rejected: the sequencer stops in the fault state and says why on [trap]/[trap_cause].

   Unimplemented on purpose: the other 253 opcodes, INTE/EI/DI, HOLD, INT, I/O and stack cycles.
   The registers other than A and PC are held at their reset values; they exist so that the
   observation ports are the ones the CPU will keep. *)
open Hardcaml
open Signal

module I = struct
  type 'a t = {
    clock : 'a;
    tick : 'a;  (** T-state enable: the CPU advances only in cycles where it is high *)
    rst_in : 'a;  (** the RESET pin (K4 [reset]); named so that Vcd.wrap does not drop it *)
    ready : 'a;
    hold : 'a;
    int_req : 'a;
    di : 'a; [@bits 8]
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    (* pins *)
    a : 'a; [@bits 16]
    dout : 'a; [@bits 8]
    dout_en : 'a;  (** [dout] is driven: the status word in T1, the data of a write from T2 *)
    sync : 'a;
    dbin : 'a;
    wr_n : 'a;
    wait_ : 'a;
    hlda : 'a;
    inte : 'a;
    (* observation ports *)
    sample : 'a;  (** the sample strobe: high in the cycle that ends a T-state *)
    acc : 'a; [@bits 8]
    bc : 'a; [@bits 16]
    de : 'a; [@bits 16]
    hl : 'a; [@bits 16]
    sp : 'a; [@bits 16]
    pc : 'a; [@bits 16]
    wz : 'a; [@bits 16]
    z : 'a;
    s : 'a;
    p : 'a;
    c : 'a;
    ac : 'a;
    tstate : 'a; [@bits 3]
    cycle : 'a; [@bits 3]
    accept : 'a;  (** a bus transfer is accepted in this T-state (T3, or the first TWH) *)
    opcode : 'a; [@bits 8]  (** the last opcode accepted *)
    trap : 'a;
    trap_cause : 'a; [@bits 2]
  }
  [@@deriving hardcaml]
end

(* T-state register *)
let idle = 0
let t1 = 1
let t2 = 2
let tw = 3
let t3 = 4
let t4 = 5
let twh = 6
let fault = 7

(* machine-cycle register *)
let c_fetch = 0
let c_read_imm = 1
let c_read_lo = 2
let c_read_hi = 3
let c_write = 4
let c_halt = 5

(* trap causes *)
let cause_opcode = 1
let cause_input = 2

(* the implemented opcodes *)
let op_mvi_a = 0x3e
let op_sta = 0x32
let op_hlt = 0x76
let reset_flags = 0x02

let create (i : _ I.t) =
  let spec = Reg_spec.create ~clock:i.clock () in
  let reg w = Always.Variable.reg ~enable:i.tick ~width:w spec in
  let t = reg 3 and cyc = reg 3 and addr = reg 16 and pc = reg 16 and wz = reg 16 in
  let a = reg 8 and ir = reg 8 and first_twh = reg 1 and cause = reg 2 in
  let bc = reg 16 and de = reg 16 and hl = reg 16 and sp = reg 16 and flags = reg 8 in
  let v = Always.Variable.value in
  let c3 n = of_int ~width:3 n in
  let in_t n = v t ==: c3 n in
  let in_c n = v cyc ==: c3 n in
  let known op = op ==:. op_mvi_a |: (op ==:. op_sta) |: (op ==:. op_hlt) in
  let reading = in_c c_fetch |: in_c c_read_imm |: in_c c_read_lo |: in_c c_read_hi in
  (* T2 and each Tw: READY decides between T3 and one more wait; the halt cycle ignores it *)
  let wait_state =
    Always.
      [
        if_ (in_c c_halt)
          [ t <--. twh; first_twh <--. 1 ]
          [
            if_ i.ready
              [
                t <--. t3;
                switch (v cyc)
                  [
                    (c3 c_fetch, [ ir <-- i.di ]);
                    (c3 c_read_imm, [ a <-- i.di ]);
                    (c3 c_read_lo, [ wz <-- (v wz).:[15, 8] @: i.di ]);
                    (c3 c_read_hi, [ wz <-- i.di @: (v wz).:[7, 0] ]);
                  ];
              ]
              [ t <--. tw ];
          ];
      ]
  in
  let next_cycle ~cycle ~address = Always.[ t <--. t1; cyc <--. cycle; addr <-- address ] in
  let t3_state =
    Always.
      [
        if_ (in_c c_fetch)
          [ if_ (known (v ir)) [ t <--. t4 ] [ t <--. fault; cause <--. cause_opcode ] ]
          [
            switch (v cyc)
              [
                (c3 c_read_imm, next_cycle ~cycle:c_fetch ~address:(v pc));
                (c3 c_read_lo, next_cycle ~cycle:c_read_hi ~address:(v pc));
                (c3 c_read_hi, next_cycle ~cycle:c_write ~address:(v wz));
                (c3 c_write, next_cycle ~cycle:c_fetch ~address:(v pc));
              ];
          ];
      ]
  in
  let t4_state =
    Always.
      [
        if_
          (v ir ==:. op_mvi_a)
          (next_cycle ~cycle:c_read_imm ~address:(v pc))
          [
            if_
              (v ir ==:. op_sta)
              (next_cycle ~cycle:c_read_lo ~address:(v pc))
              (next_cycle ~cycle:c_halt ~address:(v pc));
          ];
      ]
  in
  Always.(
    compile
      [
        if_ i.rst_in
          [
            t <--. idle;
            cyc <--. c_fetch;
            addr <--. 0;
            pc <--. 0;
            wz <--. 0;
            a <--. 0;
            ir <--. 0;
            first_twh <--. 0;
            cause <--. 0;
            bc <--. 0;
            de <--. 0;
            hl <--. 0;
            sp <--. 0;
            flags <--. reset_flags;
          ]
          [
            if_
              (i.hold |: i.int_req &: ~:(in_t fault))
              [ t <--. fault; cause <--. cause_input ]
              [
                switch (v t)
                  [
                    (c3 idle, [ t <--. t1; cyc <--. c_fetch; addr <-- v pc ]);
                    (c3 t1, [ t <--. t2; when_ reading [ pc <-- v pc +:. 1 ] ]);
                    (c3 t2, wait_state);
                    (c3 tw, wait_state);
                    (c3 t3, t3_state);
                    (c3 t4, t4_state);
                    (c3 twh, [ first_twh <--. 0 ]);
                  ];
              ];
          ];
      ]);
  let status =
    mux (v cyc)
      [
        of_int ~width:8 0xa2;
        of_int ~width:8 0x82;
        of_int ~width:8 0x82;
        of_int ~width:8 0x82;
        of_int ~width:8 0x00;
        of_int ~width:8 0x8a;
      ]
  in
  let sync = in_t t1 in
  let writing = in_c c_write &: (in_t t2 |: in_t tw |: in_t t3) in
  let flag n = (v flags).:[n, n] in
  {
    O.a = v addr;
    dout = mux2 sync status (v a);
    dout_en = sync |: writing;
    sync;
    dbin = reading &: (in_t t2 |: in_t tw);
    wr_n = ~:(in_c c_write &: (in_t tw |: in_t t3));
    wait_ = in_t tw |: in_t twh;
    hlda = gnd;
    inte = gnd;
    sample = i.tick;
    acc = v a;
    bc = v bc;
    de = v de;
    hl = v hl;
    sp = v sp;
    pc = v pc;
    wz = v wz;
    z = flag 6;
    s = flag 7;
    p = flag 2;
    c = flag 0;
    ac = flag 4;
    tstate = v t;
    cycle = v cyc;
    accept = in_t t3 |: (in_t twh &: (v first_twh ==:. 1));
    opcode = v ir;
    trap = in_t fault;
    trap_cause = v cause;
  }
