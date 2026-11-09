type kind =
  | Fetch
  | Mem_read
  | Mem_write
  | Stack_read
  | Stack_write
  | Io_read
  | Io_write
  | Int_ack
  | Halt_ack
  | Other

let kind_name = function
  | Fetch -> "fetch"
  | Mem_read -> "mem_read"
  | Mem_write -> "mem_write"
  | Stack_read -> "stack_read"
  | Stack_write -> "stack_write"
  | Io_read -> "io_read"
  | Io_write -> "io_write"
  | Int_ack -> "int_ack"
  | Halt_ack -> "halt_ack"
  | Other -> "other"

let classify = function
  | 0xa2 -> Fetch
  | 0x82 -> Mem_read
  | 0x00 -> Mem_write
  | 0x86 -> Stack_read
  | 0x04 -> Stack_write
  | 0x42 -> Io_read
  | 0x10 -> Io_write
  | 0x23 | 0x2b -> Int_ack
  | 0x8a -> Halt_ack
  | _ -> Other

let is_read st = st land 2 <> 0

type cycle = {
  epoch : int;
  seq : int;
  kind : kind;
  status : int option;
  addr : int option;
  data : int option;
  first : int;
  last : int;
  waits : int;
}

type out =
  | Reset of { epoch : int; at : int }
  | Start of {
      epoch : int;
      seq : int;
      kind : kind;
      status : int option;
      addr : int option;
      at : int;
    }
  | Done of cycle
  | Abandoned of { cycle : cycle; why : string }
  | Note of { at : int; msg : string }

type open_cycle = {
  c : cycle;
  read : bool;  (** Read-like (DBIN) as against write-like (WR̄). *)
  mutable dbin_seen : bool;
  mutable wr_seen : bool;
  mutable wdata : int option;
  mutable wlast : int;
  mutable closed : bool;
  mutable waits : int;
}

type t = { epoch : int; seq : int; prev : Frame.t option; cur : open_cycle option; in_reset : bool }

let init = { epoch = 0; seq = 0; prev = None; cur = None; in_reset = false }
let snapshot o ~last ~data = { o.c with data; last; waits = o.waits }

let finish_open o why =
  if o.closed then []
  else begin
    o.closed <- true;
    let data = if o.read then o.c.data else o.wdata in
    [ Abandoned { cycle = snapshot o ~last:(if o.wr_seen then o.wlast else o.c.first) ~data; why } ]
  end

let step t (f : Frame.t) =
  let outs = ref [] in
  let emit o = outs := o :: !outs in
  let note msg = emit (Note { at = f.n; msg }) in
  let cur = ref t.cur and epoch = ref t.epoch and seq = ref t.seq and in_reset = ref t.in_reset in
  (match f.reset with
  | None -> note "RESET is unknown (X or Z)"
  | Some true ->
      if not !in_reset then begin
        if t.prev <> None then incr epoch;
        emit (Reset { epoch = !epoch; at = f.n });
        Option.iter (fun o -> List.iter emit (finish_open o "reset")) !cur;
        cur := None;
        in_reset := true
      end
  | Some false -> in_reset := false);
  if f.reset <> Some true then begin
    match f.sync with
    | None -> note "SYNC is unknown (X or Z)"
    | Some true ->
        (* T1: a new cycle; a write the previous cycle was still holding ends here *)
        Option.iter
          (fun o ->
            if not o.closed then
              if (not o.read) && o.wr_seen then begin
                o.closed <- true;
                emit (Done (snapshot o ~last:o.wlast ~data:o.wdata))
              end
              else List.iter emit (finish_open o "the next cycle began first"))
          !cur;
        let status = f.dout in
        if status = None then note "status word on the data bus is unknown";
        let kind = match status with Some s -> classify s | None -> Other in
        let read = match status with Some s -> is_read s | None -> true in
        let c =
          {
            epoch = !epoch;
            seq = !seq;
            kind;
            status;
            addr = f.a;
            data = None;
            first = f.n;
            last = f.n;
            waits = 0;
          }
        in
        incr seq;
        emit (Start { epoch = c.epoch; seq = c.seq; kind; status; addr = f.a; at = f.n });
        cur :=
          Some
            {
              c;
              read;
              dbin_seen = false;
              wr_seen = false;
              wdata = None;
              wlast = f.n;
              closed = false;
              waits = 0;
            }
    | Some false -> (
        match !cur with
        | None -> ()
        | Some o when o.closed -> ()
        | Some o ->
            if o.c.kind = Halt_ack then begin
              match f.wait with
              | Some true ->
                  o.closed <- true;
                  emit (Done (snapshot o ~last:f.n ~data:None))
              | None -> note "WAIT is unknown (X or Z)"
              | Some false -> ()
            end
            else begin
              (match f.wait with
              | Some true -> o.waits <- o.waits + 1
              | None -> note "WAIT is unknown (X or Z)"
              | Some false -> ());
              if o.read then begin
                match f.dbin with
                | None -> note "DBIN is unknown (X or Z)"
                | Some true -> o.dbin_seen <- true
                | Some false ->
                    if o.dbin_seen then begin
                      o.closed <- true;
                      if f.din = None then note "the accepted byte on the data bus is unknown";
                      emit (Done (snapshot o ~last:f.n ~data:f.din))
                    end
              end
              else begin
                match f.wr_n with
                | None -> note "WR is unknown (X or Z)"
                | Some false ->
                    o.wr_seen <- true;
                    o.wdata <- f.dout;
                    o.wlast <- f.n;
                    if f.dout = None then note "the written byte on the data bus is unknown"
                | Some true ->
                    if o.wr_seen then begin
                      o.closed <- true;
                      emit (Done (snapshot o ~last:o.wlast ~data:o.wdata))
                    end
              end
            end)
  end;
  ({ epoch = !epoch; seq = !seq; prev = Some f; cur = !cur; in_reset = !in_reset }, List.rev !outs)

let finish t = match t.cur with None -> [] | Some o -> finish_open o "end of the capture"
