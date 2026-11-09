type swap = Fixed | Flag of int option

type regs = {
  acc : int option;
  bc : int option;
  de : int option;
  hl : int option;
  sp : int option;
  pc : int option;
  wz : int option;
  z : int option;
  s : int option;
  p : int option;
  c : int option;
  ac : int option;
  swap : swap;
}

type t = {
  n : int;
  time : int64;
  a : int option;
  dout : int option;
  din : int option;
  sync : bool option;
  dbin : bool option;
  wr_n : bool option;
  wait : bool option;
  hlda : bool option;
  inte : bool option;
  ready : bool option;
  hold : bool option;
  int_ : bool option;
  reset : bool option;
  regs : regs option;
}

let no_regs =
  {
    acc = None;
    bc = None;
    de = None;
    hl = None;
    sp = None;
    pc = None;
    wz = None;
    z = None;
    s = None;
    p = None;
    c = None;
    ac = None;
    swap = Fixed;
  }

let pp ppf f =
  let h w = function None -> String.make w '~' | Some v -> Printf.sprintf "%0*x" w v in
  let b = function None -> "~" | Some true -> "1" | Some false -> "0" in
  Format.fprintf ppf "%d %s %s %s %s %s %s %s %s %s %s %s %s %s" f.n (h 4 f.a) (h 2 f.dout)
    (h 2 f.din) (b f.sync) (b f.dbin) (b f.wr_n) (b f.wait) (b f.hlda) (b f.inte) (b f.ready)
    (b f.hold) (b f.int_) (b f.reset)
