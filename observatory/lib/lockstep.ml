type setup = {
  memory : (int * string) list;
  pc : int;
  sp : int;
  known : (int * string * string) list;
  start_regs : Regs.t option;
}

type plant = No_plant | Ult | Aux | Carry

type divergence = {
  idx : int;
  pc : int option;
  what : string;
  detail : string;
  first : int;
  last : int;
  time_first : int64 option;
  time_last : int64 option;
}

type report = {
  compared : int;
  skipped : int;
  partial : int;
  tail : int;
  halted : bool;
  divergence : divergence option;
  model_tstates : int;
  all : divergence list;
  classified : (divergence * string) list;
}

let plant_of_string = function
  | "none" -> Some No_plant
  | "ult" -> Some Ult
  | "aux" -> Some Aux
  | "carry" -> Some Carry
  | _ -> None

type access = Rd of int * int | Wr of int * int | In of int * int | Out of int * int

let pp_access ppf = function
  | Rd (a, v) -> Format.fprintf ppf "read %04x=%02x" a v
  | Wr (a, v) -> Format.fprintf ppf "write %04x=%02x" a v
  | In (a, v) -> Format.fprintf ppf "in %02x=%02x" a v
  | Out (a, v) -> Format.fprintf ppf "out %02x=%02x" a v

let show_accesses l = String.concat "; " (List.map (Format.asprintf "%a" pp_access) l)

let pp_divergence ppf d =
  Format.fprintf ppf "instruction %d%s: %s: %s (T%d-%d%s)" d.idx
    (match d.pc with Some p -> Printf.sprintf " at %04x" p | None -> "")
    d.what d.detail d.first d.last
    (match (d.time_first, d.time_last) with
    | Some a, Some b -> Printf.sprintf ", waveform time %Ld-%Ld" a b
    | _ -> "")

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  go 0

(* The capture stops after the program's last OUT, mid-way through one more fetch: not a divergence. *)
let cut_tail ~last (i : Instr.t) =
  last && (not i.complete)
  && contains i.why "end of the capture"
  && List.length i.bytes < Option.value i.expected_len ~default:1

module Make (V : Compute.VALUE with type t = int) = struct
  module C = I8080_sem.Concrete_of (V)

  let run ~all setup ~time_of (instrs : Instr.t list) =
    let next_in = ref 0xff in
    let st =
      C.create ~env:{ C.input = (fun _ -> !next_in); output = (fun _ _ -> ()) } (C.create_memory ())
    in
    List.iter (fun (a, s) -> C.poke st a s) setup.memory;
    st.C.pc <- setup.pc;
    st.C.sp <- setup.sp;
    Option.iter
      (fun (r : Regs.t) ->
        let set f v = Option.iter f v in
        set (fun v -> st.C.a <- v) r.a;
        set (fun v -> st.C.b <- v) r.b;
        set (fun v -> st.C.c <- v) r.c;
        set (fun v -> st.C.d <- v) r.d;
        set (fun v -> st.C.e <- v) r.e;
        set (fun v -> st.C.h <- v) r.h;
        set (fun v -> st.C.l <- v) r.l;
        set
          (fun f ->
            st.C.s <- (f lsr 7) land 1;
            st.C.z <- (f lsr 6) land 1;
            st.C.ac <- (f lsr 4) land 1;
            st.C.p <- (f lsr 2) land 1;
            st.C.cy <- f land 1)
          r.f)
      setup.start_regs;
    let fetched = ref [] and accesses = ref [] in
    st.C.log <-
      Some
        (function
        | C.Fetch (_, v) -> fetched := v :: !fetched
        | C.Read (a, v) -> accesses := Rd (a, v) :: !accesses
        | C.Write (a, v) -> accesses := Wr (a, v) :: !accesses
        | C.In (a, v) -> accesses := In (a, v) :: !accesses
        | C.Out (a, v) -> accesses := Out (a, v) :: !accesses);
    let skipped = ref 0 and compared = ref 0 and partial = ref 0 and tail = ref 0 in
    let total = List.length instrs in
    let div = ref None and finished = ref false and found = ref [] and classified = ref [] in
    let diverge (i : Instr.t) what detail =
      let d =
        {
          idx = i.idx;
          pc = i.pc;
          what;
          detail;
          first = i.first;
          last = i.last;
          time_first = time_of i.first;
          time_last = time_of i.last;
        }
      in
      if !div = None then div := Some d;
      found := d :: !found
    in
    (* a difference listed as known is classified and the run goes on *)
    let issue (i : Instr.t) what detail =
      let known =
        match i.bytes with
        | op :: _ ->
            List.find_map (fun (o, w, n) -> if o = op && w = what then Some n else None) setup.known
        | [] -> None
      in
      match known with
      | Some note ->
          classified :=
            ( {
                idx = i.idx;
                pc = i.pc;
                what;
                detail;
                first = i.first;
                last = i.last;
                time_first = time_of i.first;
                time_last = time_of i.last;
              },
              note )
            :: !classified;
          true
      | None ->
          diverge i what detail;
          false
    in
    let stop () = (not all) && !div <> None in
    let started = ref false in
    let observed_accesses (i : Instr.t) =
      let rec drop n l = if n <= 0 then l else match l with [] -> [] | _ :: r -> drop (n - 1) r in
      let rest = drop (List.length i.bytes) i.accesses in
      List.filter_map
        (fun (c : Bus.cycle) ->
          match (c.kind, c.addr, c.data) with
          | (Bus.Mem_read | Bus.Stack_read), Some a, Some d -> Some (Ok (Rd (a, d)))
          | (Bus.Mem_write | Bus.Stack_write), Some a, Some d -> Some (Ok (Wr (a, d)))
          | Bus.Io_read, Some a, Some d -> Some (Ok (In (a land 0xff, d)))
          | Bus.Io_write, Some a, Some d -> Some (Ok (Out (a land 0xff, d)))
          | ( ( Bus.Mem_read | Bus.Stack_read | Bus.Mem_write | Bus.Stack_write | Bus.Io_read
              | Bus.Io_write ),
              _,
              _ ) ->
              Some (Error "an address or data byte of the bus transfer is unknown")
          | _ -> None)
        rest
    in
    List.iteri
      (fun pos (i : Instr.t) ->
        if (not (stop ())) && not !finished then
          if (not !started) && i.pc <> Some setup.pc then incr skipped
          else begin
            started := true;
            let complete = i.complete in
            (if !compared = 0 then
               match i.before with
               | Some ob -> (
                   let mb = C.regs st in
                   let m =
                     Regs.of_ints ~a:mb.I8080_sem.ra ~b:mb.rb ~c:mb.rc ~d:mb.rd ~e:mb.re ~h:mb.rh
                       ~l:mb.rl ~f:mb.flags ~sp:mb.rsp ~pc:mb.rpc
                   in
                   match List.find_opt (fun n -> Regs.get ob n <> Regs.get m n) Regs.names with
                   | Some n ->
                       let show = function Some v -> Printf.sprintf "%x" v | None -> "unknown" in
                       diverge i "initial state"
                         (Printf.sprintf "register %s: observed %s, model %s" n
                            (show (Regs.get ob n))
                            (show (Regs.get m n)))
                   | None -> ())
               | None -> ());
            let partial_ok =
              (not complete) && i.why = "end of the capture"
              && match i.expected_len with Some l -> List.length i.bytes = l | None -> false
            in
            if stop () then ()
            else if cut_tail ~last:(pos = total - 1) i then incr tail
            else if (not complete) && not partial_ok then
              diverge i "capture" ("the observed instruction is incomplete: " ^ i.why)
            else begin
              let before = C.regs st in
              fetched := [];
              accesses := [];
              (match List.find_opt (fun (c : Bus.cycle) -> c.kind = Bus.Io_read) i.accesses with
              | Some { data = Some d; _ } -> next_in := d
              | _ -> ());
              let t0 = st.C.tstates in
              C.step st;
              let after = C.regs st in
              let mfetched = List.rev !fetched and maccess = List.rev !accesses in
              let mregs =
                Regs.of_ints ~a:after.I8080_sem.ra ~b:after.rb ~c:after.rc ~d:after.rd ~e:after.re
                  ~h:after.rh ~l:after.rl ~f:after.flags ~sp:after.rsp ~pc:after.rpc
              in
              (if i.pc <> Some before.I8080_sem.rpc then
                 diverge i "pc"
                   (Printf.sprintf "observed %s, model %04x"
                      (match i.pc with Some p -> Printf.sprintf "%04x" p | None -> "unknown")
                      before.rpc)
               else if i.bytes <> mfetched then
                 diverge i "bytes"
                   (Printf.sprintf "observed %s, model %s"
                      (String.concat " " (List.map (Printf.sprintf "%02x") i.bytes))
                      (String.concat " " (List.map (Printf.sprintf "%02x") mfetched)))
               else
                 let obs = observed_accesses i in
                 if List.exists Result.is_error obs then
                   diverge i "unobserved" "an address or data byte of a bus transfer is unknown"
                 else begin
                   let obs = List.map Result.get_ok obs in
                   let continue_ =
                     if obs = maccess then true
                     else if List.sort compare obs = List.sort compare maccess then
                       issue i "access order"
                         (Printf.sprintf "observed [%s], model [%s]" (show_accesses obs)
                            (show_accesses maccess))
                     else begin
                       diverge i "accesses"
                         (Printf.sprintf "observed [%s], model [%s]" (show_accesses obs)
                            (show_accesses maccess));
                       false
                     end
                   in
                   if not continue_ then ()
                   else if partial_ok then incr partial
                   else
                     match i.after with
                     | None ->
                         diverge i "unobserved"
                           "the registers after the instruction are not observed"
                     | Some oa -> (
                         match
                           List.find_opt (fun n -> Regs.get oa n <> Regs.get mregs n) Regs.names
                         with
                         | Some n ->
                             let show = function
                               | Some v -> Printf.sprintf "%x" v
                               | None -> "unknown"
                             in
                             diverge i ("reg " ^ n)
                               (Printf.sprintf "observed %s, model %s"
                                  (show (Regs.get oa n))
                                  (show (Regs.get mregs n)))
                         | None ->
                             let waits =
                               List.fold_left (fun a (c : Bus.cycle) -> a + c.waits) 0 i.accesses
                             in
                             let model_t = st.C.tstates - t0 in
                             if i.tstates - waits <> model_t then
                               diverge i "tstates"
                                 (Printf.sprintf "observed %d (%d waits), model %d" i.tstates waits
                                    model_t)
                             else incr compared)
                 end);
              if st.C.halted then finished := true
            end
          end)
      instrs;
    if not !started then (
      let d =
        {
          idx = -1;
          pc = None;
          what = "capture";
          detail = Printf.sprintf "no observed instruction starts at %04x" setup.pc;
          first = 0;
          last = 0;
          time_first = None;
          time_last = None;
        }
      in
      div := Some d;
      found := d :: !found);
    {
      compared = !compared;
      skipped = !skipped;
      partial = !partial;
      tail = !tail;
      halted = st.C.halted;
      divergence = !div;
      model_tstates = st.C.tstates;
      all = List.rev !found;
      classified = List.rev !classified;
    }
end

module Real = Make (Compute.Concrete)

module Ult = Make (struct
  include Compute.Concrete

  let ult a b = if a <= b then 1 else 0
end)

module Aux = Make (struct
  include Compute.Concrete

  let extract a ~hi ~lo = if hi = 4 && lo = 4 then 0 else Compute.Concrete.extract a ~hi ~lo
end)

module Carry = Make (struct
  include Compute.Concrete

  let add ~w a b = if w = 9 && a + b = 0x100 then 0 else Compute.Concrete.add ~w a b
end)

let run ?(plant = No_plant) ?(all = false) setup ~time_of instrs =
  match plant with
  | No_plant -> Real.run ~all setup ~time_of instrs
  | Ult -> Ult.run ~all setup ~time_of instrs
  | Aux -> Aux.run ~all setup ~time_of instrs
  | Carry -> Carry.run ~all setup ~time_of instrs
