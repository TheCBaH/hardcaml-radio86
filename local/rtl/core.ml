(* The Intel 8080 in Hardcaml: all 256 opcodes, with I/O, HOLD/HLDA and INT/INTA.

   One enabled clock cycle is one T-state (K2). The pins are a function of the state registers only
   (Moore), so the values seen in the cycle that ends T-state k are the pins of T-state k together with
   the inputs applied during it: this is the sample of the K4 pin trace.

   Sequencing: an instruction is a list of bus-visible machine cycles ({!Plan}); a ROM indexed by the
   opcode and the cycle number gives each cycle's status word, length and address source. The T-states
   past T3 of a cycle are the internal work the 8080 does after the bus transfer. All register effects
   of a cycle happen in its last T-state, and the address of the next cycle is formed in its T1 from the
   registers that effect has just written. *)
open Hardcaml
open Signal
module A = Alu.Make (Signal)

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
  }
  [@@deriving hardcaml]
end

(* T-state register; [t4] stands for every T-state after T3 of a cycle *)
let idle = 0
let t1 = 1
let t2 = 2
let tw = 3
let t3 = 4
let t4 = 5
let twh = 6
let thold = 7
let reset_flags = 0x02

let create (i : _ I.t) =
  (* RESET is the synchronous clear of every register: it does not wait for [tick] *)
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.rst_in () in
  let reg ?clear_to w =
    let spec =
      match clear_to with
      | None -> spec
      | Some v -> Reg_spec.override spec ~clear_to:(of_int ~width:w v)
    in
    Always.Variable.reg ~enable:i.tick ~width:w spec
  in
  let t = reg 3 and cyc = reg 3 and xc = reg 4 and addr = reg 16 in
  let pc = reg 16 and sp = reg 16 and wz = reg 16 in
  let ir = reg 8 and dlat = reg 8 and tmp = reg 8 and flags = reg ~clear_to:reset_flags 8 in
  let ra = reg 8 and rb = reg 8 and rc = reg 8 and rd = reg 8 and re = reg 8 in
  let rh = reg 8 and rl = reg 8 in
  let inte = reg 1 and first_twh = reg 1 in
  let hlda = reg 1 and hacc = reg 1 and iack = reg 1 and from_halt = reg 1 and hexit = reg 2 in
  let v = Always.Variable.value in
  let in_t n = v t ==:. n in
  let irv = v ir and cycv = v cyc in
  (* the sequencing ROM: status word, length, address source and "last cycle" per opcode and cycle *)
  let rom ~width field =
    mux cycv
      (List.init Plan.max_cycles (fun k ->
           mux irv (List.init 256 (fun op -> of_int ~width (field op k)))))
  in
  let rom_status = rom ~width:8 (fun op k -> (Plan.cycle op k).status) in
  let len = rom ~width:4 (fun op k -> (Plan.cycle op k).len) in
  let src = rom ~width:3 (fun op k -> Plan.src_code (Plan.cycle op k).src) in
  (* an instruction that INTA supplies: its fetch is the acknowledge cycle (23, or 2b out of a halt), its
     other bytes come with 03, and PC does not move *)
  let status =
    let c = of_int ~width:8 in
    mux2
      (v iack &: (cycv ==:. 0))
      (mux2 (v from_halt) (c 0x2b) (c 0x23))
      (mux2 (v iack &: (rom_status ==:. 0x82) &: (src ==:. 0)) (c 0x03) rom_status)
  in
  let last = rom ~width:1 (fun op k -> if Plan.is_last op k then 1 else 0) in
  (* what the opcode is; [&:], [|:] and [==:] share one precedence level, so every mix is bracketed *)
  let hi2 = irv.:[7, 6] and r3 = irv.:[2, 0] and d3 = irv.:[5, 3] and rp = irv.:[5, 4] in
  let hi_is n = hi2 ==:. n and r_is n = r3 ==:. n and d_is n = d3 ==:. n in
  let cyc_is n = cycv ==:. n in
  let odd_d = d3.:[0, 0] in
  let ( &&: ) = ( &: ) and ( ||: ) = ( |: ) in
  let is_mov = hi_is 1 &&: ~:(irv ==:. 0x76) in
  let is_alu_r = hi_is 2 and is_alu_i = hi_is 3 &&: r_is 6 in
  let is_lxi = hi_is 0 &&: r_is 1 &&: ~:odd_d and is_dad = hi_is 0 &&: r_is 1 &&: odd_d in
  let is_ld_st = hi_is 0 &&: r_is 2 in
  let is_ldax = is_ld_st &&: (d_is 1 ||: d_is 3) in
  let is_shld = is_ld_st &&: d_is 4 and is_lhld = is_ld_st &&: d_is 5 in
  let is_lda = is_ld_st &&: d_is 7 in
  let is_incdec_rp = hi_is 0 &&: r_is 3 in
  let is_inr = hi_is 0 &&: r_is 4 and is_dcr = hi_is 0 &&: r_is 5 in
  let is_mvi = hi_is 0 &&: r_is 6 and is_rot = hi_is 0 &&: r_is 7 in
  let is_rcc = hi_is 3 &&: r_is 0 in
  let is_pop = hi_is 3 &&: r_is 1 &&: ~:odd_d in
  let is_ret = hi_is 3 &&: r_is 1 &&: (d_is 1 ||: d_is 3) in
  let is_pchl = hi_is 3 &&: r_is 1 &&: d_is 5 and is_sphl = hi_is 3 &&: r_is 1 &&: d_is 7 in
  let is_jcc = hi_is 3 &&: r_is 2 in
  let is_jmp = hi_is 3 &&: r_is 3 &&: (d_is 0 ||: d_is 1) in
  let is_out = hi_is 3 &&: r_is 3 &&: d_is 2 and is_in = hi_is 3 &&: r_is 3 &&: d_is 3 in
  let is_xthl = hi_is 3 &&: r_is 3 &&: d_is 4 and is_xchg = hi_is 3 &&: r_is 3 &&: d_is 5 in
  let is_di = hi_is 3 &&: r_is 3 &&: d_is 6 and is_ei = hi_is 3 &&: r_is 3 &&: d_is 7 in
  let is_ccc = hi_is 3 &&: r_is 4 in
  let is_push = hi_is 3 &&: r_is 5 &&: ~:odd_d and is_call = hi_is 3 &&: r_is 5 &&: odd_d in
  let is_rst = hi_is 3 &&: r_is 7 in
  (* register file; selector 6 is memory, which reads as the byte just fetched *)
  let get8 sel = mux sel [ v rb; v rc; v rd; v re; v rh; v rl; v dlat; v ra ] in
  let set8 sel value =
    List.concat_map
      (fun (n, r) -> Always.[ when_ (sel ==:. n) [ r <-- value ] ])
      [ (0, rb); (1, rc); (2, rd); (3, re); (4, rh); (5, rl); (7, ra) ]
  in
  let bc = v rb @: v rc and de = v rd @: v re and hl = v rh @: v rl in
  let get_pair sel = mux sel [ bc; de; hl; v sp ] in
  let set_pair sel value =
    let hi = value.:[15, 8] and lo = value.:[7, 0] in
    Always.
      [
        when_ (sel ==:. 0) [ rb <-- hi; rc <-- lo ];
        when_ (sel ==:. 1) [ rd <-- hi; re <-- lo ];
        when_ (sel ==:. 2) [ rh <-- hi; rl <-- lo ];
        when_ (sel ==:. 3) [ sp <-- value ];
      ]
  in
  let f = A.unpack (v flags) in
  let fz = f.z and fs = f.s and fp = f.p and fc = f.c and fac = f.ac in
  let cond = mux d3 [ ~:fz; fz; ~:fc; fc; ~:fp; fp; ~:fs; fs ] in
  (* the arithmetic ({!Alu}); operand 6 is the byte just read, an immediate included *)
  let a = v ra in
  let alu_res, alu_f = A.alu ~op:d3 ~a ~y:(get8 r3) ~c:fc in
  let incdec_res, incdec_f = A.incdec ~dec:(r_is 5) ~x:(get8 d3) ~c:fc in
  let rot_res, rot_c = A.rotate ~op:d3.:[1, 0] ~a ~c:fc in
  let daa_res, daa_f = A.daa ~a ~ac:fac ~c:fc in
  let alu_flags = A.pack alu_f and incdec_flags = A.pack incdec_f and daa_flags = A.pack daa_f in
  let dad17 = uresize hl 17 +: uresize (get_pair rp) 17 in
  (* what a write cycle puts on the data bus *)
  let push_hi = mux rp [ v rb; v rd; v rh; v ra ]
  and push_lo = mux rp [ v rc; v re; v rl; v flags ] in
  let ret_hi = is_rst &&: cyc_is 1 ||: (~:is_rst &&: cyc_is 3) in
  let wdata =
    List.fold_right
      (fun (c, x) rest -> mux2 c x rest)
      [
        (is_ld_st &&: ~:is_shld ||: is_out, a);
        (is_mov, get8 r3);
        (is_mvi ||: is_inr ||: is_dcr, v tmp);
        (is_shld &&: cyc_is 3, v rl);
        (is_shld, v rh);
        (is_push &&: cyc_is 1, push_hi);
        (is_push, push_lo);
        (is_call ||: is_ccc ||: is_rst &&: ret_hi, (v pc).:[15, 8]);
        (is_call ||: is_ccc ||: is_rst, (v pc).:[7, 0]);
        (is_xthl &&: cyc_is 3, v rh);
      ]
      (v rl)
  in
  (* the cycle in progress *)
  let halt_cycle = status ==:. 0x8a in
  let rd_like = status.:[1, 1] &&: ~:halt_cycle in
  let wr_like = ~:(status.:[1, 1]) in
  let src_sel = mux2 (cyc_is 0) (zero 3) src in
  let a_now = mux src_sel [ v pc; hl; bc; de; v sp; v wz ] in
  let early_end = is_rcc &&: cyc_is 0 ||: (is_ccc &&: cyc_is 2) &&: ~:cond in
  let finished = last ||: early_end in
  let fin = in_t t3 &&: (len ==:. 3) ||: (in_t t4 &&: (v xc ==:. 0)) in
  (* HOLD is taken in T2 (or the last Tw) of a cycle: HLDA rises with T3 of a read and after T3 of a
     write, the cycle runs to its end and the core then idles in [thold] until HOLD falls. HLDA follows
     a HOLD that falls earlier, and the core goes on. *)
  let hold_track =
    Always.[ when_ (v hacc) [ hlda <-- i.hold; when_ ~:(i.hold) [ hacc <--. 0 ] ] ]
  in
  (* INT is sampled in the last T-state of an instruction, not after EI or DI and not while a HOLD is
     being acknowledged *)
  let int_take = i.int_req &&: v inte &&: ~:(is_ei ||: is_di) &&: ~:(v hacc) in
  let advance =
    Always.
      [
        when_ fin
          [
            if_ (v hacc &&: i.hold) [ t <--. thold ] [ t <--. t1 ];
            if_ finished
              [ cyc <--. 0; iack <-- int_take; from_halt <--. 0; when_ int_take [ inte <--. 0 ] ]
              [ cyc <-- cycv +:. 1 ];
          ];
      ]
  in
  (* halted: HLDA follows HOLD; an INT with INTE set ends the halt two T-states later with an
     acknowledge cycle *)
  let halted =
    Always.
      [
        hlda <-- i.hold;
        if_
          (v hexit ==:. 0)
          [ when_ (i.int_req &&: v inte &&: ~:(v hlda)) [ hexit <--. 1 ] ]
          [
            if_
              (v hexit ==:. 1)
              [ hexit <--. 2 ]
              [ hexit <--. 0; t <--. t1; cyc <--. 0; iack <--. 1; from_halt <--. 1; inte <--. 0 ];
          ];
      ]
  in
  (* T2 and each Tw: READY decides between T3 and one more wait; the halt cycle ignores it *)
  let wait_state =
    Always.
      [
        if_ halt_cycle
          ([ t <--. twh; first_twh <--. 1 ] @ halted)
          [
            if_ i.ready
              [
                t <--. t3;
                when_ rd_like [ dlat <-- i.di ];
                when_ (cyc_is 0) [ ir <-- i.di ];
                when_ i.hold [ hacc <--. 1; when_ rd_like [ hlda <--. 1 ] ];
              ]
              [ t <--. tw ];
          ];
      ]
  in
  let t3_state = Always.[ when_ (len <>:. 3) [ t <--. t4; xc <-- len -:. 4 ]; proc hold_track ] in
  let t4_state = Always.[ when_ (v xc <>:. 0) [ xc <-- v xc -:. 1 ]; proc hold_track ] in
  (* the effects of a cycle, in its last T-state *)
  let at n = fin &&: cyc_is n in
  let pc_next = v dlat @: (v wz).:[7, 0] in
  let wz_lo = Always.[ wz <-- (v wz).:[15, 8] @: v dlat ] in
  let wz_hi = Always.[ wz <-- v dlat @: (v wz).:[7, 0] ] in
  let sp_pop = Always.[ sp <-- v sp +:. 1 ] and sp_push = Always.[ sp <-- v sp -:. 1 ] in
  let take = ~:(is_jcc ||: is_ccc) ||: cond in
  let wz16 = is_lxi ||: (is_ld_st &&: d3.:[2, 2]) ||: is_jcc ||: is_jmp ||: is_ccc ||: is_call in
  let other_flags_unchanged ~c = A.pack { f with c } in
  let alu_now =
    at 0 &&: is_alu_r &&: ~:(r_is 6) ||: (at 1 &&: (is_alu_r &&: r_is 6 ||: is_alu_i))
  in
  let pop_to lo_hi =
    Always.
      [
        when_ (rp ==:. 0) [ (if lo_hi then rb else rc) <-- v dlat ];
        when_ (rp ==:. 1) [ (if lo_hi then rd else re) <-- v dlat ];
        when_ (rp ==:. 2) [ (if lo_hi then rh else rl) <-- v dlat ];
        when_ (rp ==:. 3)
          (if lo_hi then [ ra <-- v dlat ] else [ flags <-- (v dlat &:. 0xd5 |:. 0x02) ]);
      ]
  in
  let effects =
    Always.
      [
        (* operands from the instruction stream and the I/O port number *)
        when_ (at 1 &&: wz16) wz_lo;
        when_ (at 2 &&: wz16) wz_hi;
        when_ (at 1 &&: (is_in ||: is_out)) [ wz <-- v dlat @: v dlat ];
        (* moves *)
        when_ (at 0 &&: is_mov &&: ~:(r_is 6) &&: ~:(d_is 6)) (set8 d3 (get8 r3));
        when_ (at 1 &&: is_mov &&: r_is 6) (set8 d3 (v dlat));
        when_ (at 1 &&: is_mvi &&: ~:(d_is 6)) (set8 d3 (v dlat));
        when_ (at 1 &&: is_mvi &&: d_is 6) [ tmp <-- v dlat ];
        when_ (at 1 &&: is_ldax) [ ra <-- v dlat ];
        when_ (at 3 &&: is_lda) [ ra <-- v dlat ];
        when_ (at 2 &&: is_in) [ ra <-- v dlat ];
        when_ (at 3 &&: is_lhld) [ rl <-- v dlat; wz <-- v wz +:. 1 ];
        when_ (at 4 &&: is_lhld) [ rh <-- v dlat ];
        when_ (at 3 &&: is_shld) [ wz <-- v wz +:. 1 ];
        when_ (at 0 &&: is_xchg) [ rd <-- v rh; re <-- v rl; rh <-- v rd; rl <-- v re ];
        when_ (at 0 &&: is_sphl) [ sp <-- hl ];
        (* arithmetic *)
        when_ alu_now [ flags <-- alu_flags; when_ ~:(d_is 7) [ ra <-- alu_res ] ];
        when_
          (at 0 &&: (is_inr ||: is_dcr) &&: ~:(d_is 6))
          ((flags <-- incdec_flags) :: set8 d3 incdec_res);
        when_
          (at 1 &&: (is_inr ||: is_dcr) &&: d_is 6)
          [ flags <-- incdec_flags; tmp <-- incdec_res ];
        when_
          (at 0 &&: is_incdec_rp)
          (set_pair rp (mux2 odd_d (get_pair rp -:. 1) (get_pair rp +:. 1)));
        when_
          (at 0 &&: is_dad)
          [
            rh <-- dad17.:[15, 8];
            rl <-- dad17.:[7, 0];
            flags <-- other_flags_unchanged ~c:dad17.:[16, 16];
          ];
        when_ (at 2 &&: is_lxi) (set_pair rp pc_next);
        when_
          (at 0 &&: is_rot &&: (d3.:[2, 2] ==:. 0))
          [ ra <-- rot_res; flags <-- other_flags_unchanged ~c:rot_c ];
        when_ (at 0 &&: is_rot &&: d_is 4) [ ra <-- daa_res; flags <-- daa_flags ];
        when_ (at 0 &&: is_rot &&: d_is 5) [ ra <-- ~:a ];
        when_ (at 0 &&: is_rot &&: d_is 6) [ flags <-- other_flags_unchanged ~c:vdd ];
        when_ (at 0 &&: is_rot &&: d_is 7) [ flags <-- other_flags_unchanged ~c:~:fc ];
        when_ (at 0 &&: is_di) [ inte <--. 0 ];
        when_ (at 0 &&: is_ei) [ inte <--. 1 ];
        (* jumps, calls, returns and the stack *)
        when_ (at 2 &&: (is_jcc ||: is_jmp) &&: take) [ pc <-- pc_next ];
        when_ (at 0 &&: is_pchl) [ pc <-- hl ];
        when_ (at 2 &&: (is_call ||: is_ccc) &&: take) sp_push;
        when_ (at 3 &&: (is_call ||: is_ccc)) sp_push;
        when_ (at 4 &&: (is_call ||: is_ccc)) [ pc <-- v wz ];
        when_ (at 0 &&: (is_push ||: is_rst)) sp_push;
        when_ (at 1 &&: (is_push ||: is_rst)) sp_push;
        when_ (at 2 &&: is_rst) [ pc <-- zero 10 @: d3 @: zero 3 ];
        when_ (at 1 &&: (is_ret ||: is_rcc ||: is_xthl)) (wz_lo @ sp_pop);
        when_ (at 2 &&: (is_ret ||: is_rcc)) ([ pc <-- pc_next ] @ sp_pop);
        when_ (at 1 &&: is_pop) (sp_pop @ pop_to false);
        when_ (at 2 &&: is_pop) (sp_pop @ pop_to true);
        when_ (at 2 &&: is_xthl) wz_hi;
        when_ (at 3 &&: is_xthl) sp_push;
        when_ (at 4 &&: is_xthl) [ rh <-- (v wz).:[15, 8]; rl <-- (v wz).:[7, 0] ];
      ]
  in
  Always.(
    compile
      [
        switch (v t)
          [
            (of_int ~width:3 idle, [ t <--. t1; cyc <--. 0 ]);
            ( of_int ~width:3 t1,
              [
                t <--. t2;
                addr <-- a_now;
                when_ (rd_like &: (src_sel ==:. 0) &: ~:(v iack)) [ pc <-- v pc +:. 1 ];
              ] );
            (of_int ~width:3 t2, wait_state);
            (of_int ~width:3 tw, wait_state);
            (of_int ~width:3 t3, t3_state);
            (of_int ~width:3 t4, t4_state);
            (of_int ~width:3 twh, (first_twh <--. 0) :: halted);
            (of_int ~width:3 thold, [ when_ ~:(i.hold) [ t <--. t1; hlda <--. 0; hacc <--. 0 ] ]);
          ];
        proc advance;
        proc effects;
      ]);
  let sync = in_t t1 in
  let writing = wr_like &&: (in_t t2 ||: in_t tw ||: in_t t3) in
  {
    O.a = mux2 sync a_now (v addr);
    dout = mux2 sync status wdata;
    dout_en = sync ||: writing;
    sync;
    dbin = rd_like &&: (in_t t2 ||: in_t tw);
    wr_n = ~:(wr_like &&: (in_t tw ||: in_t t3));
    wait_ = in_t tw ||: in_t twh;
    hlda = v hlda;
    inte = v inte;
    sample = i.tick;
    acc = v ra;
    bc;
    de;
    hl;
    sp = v sp;
    pc = v pc;
    wz = v wz;
    z = fz;
    s = fs;
    p = fp;
    c = fc;
    ac = fac;
    tstate = v t;
    cycle = v cyc;
    accept = in_t t3 ||: (in_t twh &&: (v first_twh ==:. 1));
    opcode = v ir;
  }
