type t = {
  idx : int;
  epoch : int;
  pc : int option;
  bytes : int list;
  expected_len : int option;
  text : string option;
  first : int;
  last : int;
  tstates : int;
  accesses : Bus.cycle list;
  before : Regs.t option;
  after : Regs.t option;
  complete : bool;
  why : string;
}

type out =
  | Instruction of t
  | Halt_evidence of { first : int; samples : (int * int option * int option) list }
  | Note of { at : int; msg : string }

type open_instr = {
  o_pc : int option;
  o_epoch : int;
  o_first : int;
  mutable o_bytes : int list;  (** Reversed. *)
  mutable o_len : int option;
  mutable o_acc : Bus.cycle list;  (** Reversed. *)
  mutable o_before : Regs.t option;
  mutable o_last : int;
  mutable o_bad : string;  (** Non-empty: the instruction cannot be complete. *)
  mutable o_snapped : bool;  (** [o_before] has been read. *)
}

type halt_window = { h_first : int; mutable h_samples : (int * int option * int option) list }

type state = {
  rule : Manifest.snapshot;
  idx : int;
  cur : open_instr option;
  pending : (open_instr * int) option;
      (** The previous instruction, closed when the next fetch's registers are read, with its index. *)
  halt : halt_window option;
}

let init rule = { rule; idx = 0; cur = None; pending = None; halt = None }

let describe bytes ~origin =
  match bytes with
  | [] -> (None, None)
  | op :: _ ->
      let len = I8080_ops.timings.(op land 0xff).I8080_ops.length in
      if List.length bytes < len then (Some len, None)
      else begin
        let raw = String.init len (fun i -> Char.chr (List.nth bytes i land 0xff)) in
        match I8080_text.canonical ~origin raw with
        | Error _ -> (Some len, None)
        | Ok c -> (
            match String.split_on_char '\n' c with
            | _ :: line :: _ -> (Some len, Some (String.trim line))
            | _ -> (Some len, None))
      end

let close idx o ~after ~complete ~why =
  let bytes = List.rev o.o_bytes in
  let origin = Option.value o.o_pc ~default:0 in
  let expected_len, text = describe bytes ~origin in
  let complete = complete && o.o_bad = "" && text <> None in
  let why =
    if complete then ""
    else if o.o_bad <> "" then o.o_bad
    else if why <> "" then why
    else "bytes are incomplete"
  in
  {
    idx;
    epoch = o.o_epoch;
    pc = o.o_pc;
    bytes;
    expected_len;
    text;
    first = o.o_first;
    last = o.o_last;
    tstates = o.o_last - o.o_first + 1;
    accesses = List.rev o.o_acc;
    before = o.o_before;
    after = (if complete then after else None);
    complete;
    why;
  }

let regs_of (f : Frame.t) = Option.map Regs.of_frame f.regs

let step st (f : Frame.t) (outs : Bus.out list) =
  let res = ref [] in
  let emit o = res := o :: !res in
  let cur = ref st.cur and idx = ref st.idx and halt = ref st.halt and pending = ref st.pending in
  let now = regs_of f in
  let flush_halt () =
    Option.iter
      (fun h -> emit (Halt_evidence { first = h.h_first; samples = List.rev h.h_samples }))
      !halt;
    halt := None
  in
  let close_pending ~after ~why =
    Option.iter
      (fun (o, i) ->
        emit (Instruction (close i o ~after ~complete:(after <> None || why = "") ~why)))
      !pending;
    pending := None
  in
  let finish_cur ~after ~complete ~why =
    Option.iter
      (fun o ->
        emit (Instruction (close !idx o ~after ~complete ~why));
        incr idx)
      !cur;
    cur := None
  in
  (* the registers as the next instruction's fetch finds them: PC is the fetch address *)
  let snapshot ~addr ~advanced =
    match now with
    | None -> None
    | Some r ->
        let rg = r in
        let pc =
          match (addr, rg.pc) with
          | Some a, Some p ->
              if advanced && p <> (a + 1) land 0xffff then
                emit
                  (Note
                     {
                       at = f.n;
                       msg =
                         Printf.sprintf "internal PC %04x is not the fetch address %04x plus one" p
                           a;
                     });
              Some a
          | Some a, None -> Some a
          | None, p -> p
        in
        Some { rg with pc }
  in
  List.iter
    (fun (o : Bus.out) ->
      match o with
      | Bus.Reset _ ->
          flush_halt ();
          close_pending ~after:None ~why:"reset before the next fetch";
          finish_cur ~after:None ~complete:false ~why:"reset"
      | Bus.Note _ -> ()
      | Bus.Start { kind = Bus.Fetch | Bus.Int_ack; addr; at; epoch; _ } ->
          flush_halt ();
          close_pending ~after:None ~why:"the next fetch did not finish";
          (match !cur with
          | Some prev ->
              prev.o_last <- at - 1;
              if st.rule = Manifest.Fetch_t1 then begin
                (* too early on vm80a (see Manifest); kept to show it *)
                let snap = snapshot ~addr ~advanced:false in
                emit (Instruction (close !idx prev ~after:snap ~complete:true ~why:""));
                incr idx;
                cur := None
              end
              else begin
                pending := Some (prev, !idx);
                incr idx;
                cur := None
              end
          | None -> ());
          let snapped = st.rule = Manifest.Fetch_t1 in
          cur :=
            Some
              {
                o_pc = addr;
                o_epoch = epoch;
                o_first = at;
                o_bytes = [];
                o_len = None;
                o_acc = [];
                o_before = (if snapped then snapshot ~addr ~advanced:false else None);
                o_last = at;
                o_bad = "";
                o_snapped = snapped;
              }
      | Bus.Start { kind = Bus.Halt_ack; at; _ } -> halt := Some { h_first = at; h_samples = [] }
      | Bus.Start _ -> ()
      | Bus.Done c -> (
          match !cur with
          | None ->
              emit
                (Note
                   {
                     at = c.last;
                     msg = "a bus transfer with no instruction open (capture began mid-instruction)";
                   })
          | Some o -> (
              o.o_acc <- c :: o.o_acc;
              o.o_last <- c.last;
              match c.kind with
              | Bus.Halt_ack ->
                  close_pending ~after:None ~why:"the next fetch did not finish";
                  finish_cur ~after:(snapshot ~addr:None ~advanced:false) ~complete:true ~why:""
              | Bus.Fetch | Bus.Int_ack ->
                  (match c.data with
                  | Some b ->
                      o.o_bytes <- [ b ];
                      o.o_len <- Some I8080_ops.timings.(b land 0xff).I8080_ops.length
                  | None -> o.o_bad <- "the opcode byte is unknown");
                  if not o.o_snapped then begin
                    o.o_snapped <- true;
                    let snap = snapshot ~addr:c.addr ~advanced:(c.kind = Bus.Fetch) in
                    o.o_before <- snap;
                    close_pending ~after:snap ~why:""
                  end
              | Bus.Mem_read | Bus.Other -> (
                  match (o.o_len, c.data) with
                  | Some len, Some b when List.length o.o_bytes < len -> o.o_bytes <- b :: o.o_bytes
                  | Some len, None when List.length o.o_bytes < len ->
                      o.o_bad <- "an operand byte is unknown"
                  | _ -> ())
              | _ -> ()))
      | Bus.Abandoned { cycle; why } -> (
          match !cur with
          | Some o ->
              o.o_acc <- cycle :: o.o_acc;
              if o.o_bad = "" then
                o.o_bad <-
                  Printf.sprintf "%s cycle at T-state %d abandoned: %s" (Bus.kind_name cycle.kind)
                    cycle.first why
          | None -> ()))
    outs;
  (match !halt with
  | Some h when List.length h.h_samples < 6 ->
      h.h_samples <- (f.n, f.a, Option.bind f.regs (fun r -> r.Frame.pc)) :: h.h_samples
  | _ -> ());
  ({ st with idx = !idx; cur = !cur; pending = !pending; halt = !halt }, List.rev !res)

let finish st (outs : Bus.out list) =
  let res = ref [] in
  Option.iter
    (fun o ->
      List.iter
        (function
          | Bus.Abandoned { cycle; why } ->
              o.o_acc <- cycle :: o.o_acc;
              if o.o_bad = "" then
                o.o_bad <-
                  Printf.sprintf "%s cycle at T-state %d abandoned: %s" (Bus.kind_name cycle.kind)
                    cycle.first why
          | _ -> ())
        outs)
    st.cur;
  Option.iter
    (fun h -> res := Halt_evidence { first = h.h_first; samples = List.rev h.h_samples } :: !res)
    st.halt;
  Option.iter
    (fun (o, i) ->
      res := Instruction (close i o ~after:None ~complete:false ~why:"end of the capture") :: !res)
    st.pending;
  Option.iter
    (fun o ->
      res :=
        Instruction (close st.idx o ~after:None ~complete:false ~why:"end of the capture") :: !res)
    st.cur;
  List.rev !res
