type t = {
  a : int option;
  b : int option;
  c : int option;
  d : int option;
  e : int option;
  h : int option;
  l : int option;
  f : int option;
  sp : int option;
  pc : int option;
}

let names = [ "a"; "b"; "c"; "d"; "e"; "h"; "l"; "f"; "sp"; "pc" ]

let get t = function
  | "a" -> t.a
  | "b" -> t.b
  | "c" -> t.c
  | "d" -> t.d
  | "e" -> t.e
  | "h" -> t.h
  | "l" -> t.l
  | "f" -> t.f
  | "sp" -> t.sp
  | "pc" -> t.pc
  | n -> invalid_arg ("Regs.get " ^ n)

let unknown =
  {
    a = None;
    b = None;
    c = None;
    d = None;
    e = None;
    h = None;
    l = None;
    f = None;
    sp = None;
    pc = None;
  }

let flags_of ~z ~s ~p ~c ~ac = (s lsl 7) lor (z lsl 6) lor (ac lsl 4) lor (p lsl 2) lor 2 lor c

let of_frame (r : Frame.regs) =
  let hi = Option.map (fun v -> (v lsr 8) land 0xff) and lo = Option.map (fun v -> v land 0xff) in
  let de, hl =
    match r.swap with
    | Frame.Fixed -> (r.de, r.hl)
    | Frame.Flag (Some 0) -> (r.hl, r.de)
    | Frame.Flag (Some _) -> (r.de, r.hl)
    | Frame.Flag None -> (None, None)
  in
  let f =
    match (r.z, r.s, r.p, r.c, r.ac) with
    | Some z, Some s, Some p, Some c, Some ac -> Some (flags_of ~z ~s ~p ~c ~ac)
    | _ -> None
  in
  {
    a = r.acc;
    b = hi r.bc;
    c = lo r.bc;
    d = hi de;
    e = lo de;
    h = hi hl;
    l = lo hl;
    f;
    sp = r.sp;
    pc = r.pc;
  }

let of_ints ~a ~b ~c ~d ~e ~h ~l ~f ~sp ~pc =
  {
    a = Some a;
    b = Some b;
    c = Some c;
    d = Some d;
    e = Some e;
    h = Some h;
    l = Some l;
    f = Some f;
    sp = Some sp;
    pc = Some pc;
  }

let changes x y =
  List.filter_map
    (fun n ->
      let u = get x n and v = get y n in
      if u = v then None else Some (n, u, v))
    names
