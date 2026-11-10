let k =
  [|
    0x428a2f98;
    0x71374491;
    0xb5c0fbcf;
    0xe9b5dba5;
    0x3956c25b;
    0x59f111f1;
    0x923f82a4;
    0xab1c5ed5;
    0xd807aa98;
    0x12835b01;
    0x243185be;
    0x550c7dc3;
    0x72be5d74;
    0x80deb1fe;
    0x9bdc06a7;
    0xc19bf174;
    0xe49b69c1;
    0xefbe4786;
    0x0fc19dc6;
    0x240ca1cc;
    0x2de92c6f;
    0x4a7484aa;
    0x5cb0a9dc;
    0x76f988da;
    0x983e5152;
    0xa831c66d;
    0xb00327c8;
    0xbf597fc7;
    0xc6e00bf3;
    0xd5a79147;
    0x06ca6351;
    0x14292967;
    0x27b70a85;
    0x2e1b2138;
    0x4d2c6dfc;
    0x53380d13;
    0x650a7354;
    0x766a0abb;
    0x81c2c92e;
    0x92722c85;
    0xa2bfe8a1;
    0xa81a664b;
    0xc24b8b70;
    0xc76c51a3;
    0xd192e819;
    0xd6990624;
    0xf40e3585;
    0x106aa070;
    0x19a4c116;
    0x1e376c08;
    0x2748774c;
    0x34b0bcb5;
    0x391c0cb3;
    0x4ed8aa4a;
    0x5b9cca4f;
    0x682e6ff3;
    0x748f82ee;
    0x78a5636f;
    0x84c87814;
    0x8cc70208;
    0x90befffa;
    0xa4506ceb;
    0xbef9a3f7;
    0xc67178f2;
  |]

let m = 0xffffffff
let rotr x n = (x lsr n) lor (x lsl (32 - n)) land m

let hex s =
  let len = String.length s in
  let padded_len = (((len + 8) / 64) + 1) * 64 in
  let b = Bytes.make padded_len '\000' in
  Bytes.blit_string s 0 b 0 len;
  Bytes.set b len '\x80';
  let bits = len * 8 in
  for i = 0 to 7 do
    Bytes.set b (padded_len - 1 - i) (Char.chr ((bits lsr (8 * i)) land 0xff))
  done;
  let h =
    [|
      0x6a09e667; 0xbb67ae85; 0x3c6ef372; 0xa54ff53a; 0x510e527f; 0x9b05688c; 0x1f83d9ab; 0x5be0cd19;
    |]
  in
  let w = Array.make 64 0 in
  for blk = 0 to (padded_len / 64) - 1 do
    for t = 0 to 15 do
      let o = (blk * 64) + (t * 4) in
      w.(t) <-
        (Char.code (Bytes.get b o) lsl 24)
        lor (Char.code (Bytes.get b (o + 1)) lsl 16)
        lor (Char.code (Bytes.get b (o + 2)) lsl 8)
        lor Char.code (Bytes.get b (o + 3))
    done;
    for t = 16 to 63 do
      let s0 = rotr w.(t - 15) 7 lxor rotr w.(t - 15) 18 lxor (w.(t - 15) lsr 3) in
      let s1 = rotr w.(t - 2) 17 lxor rotr w.(t - 2) 19 lxor (w.(t - 2) lsr 10) in
      w.(t) <- (w.(t - 16) + s0 + w.(t - 7) + s1) land m
    done;
    let a = ref h.(0) and bb = ref h.(1) and c = ref h.(2) and d = ref h.(3) in
    let e = ref h.(4) and f = ref h.(5) and g = ref h.(6) and hh = ref h.(7) in
    for t = 0 to 63 do
      let s1 = rotr !e 6 lxor rotr !e 11 lxor rotr !e 25 in
      let ch = !e land !f lxor (lnot !e land m land !g) in
      let t1 = (!hh + s1 + ch + k.(t) + w.(t)) land m in
      let s0 = rotr !a 2 lxor rotr !a 13 lxor rotr !a 22 in
      let maj = !a land !bb lxor (!a land !c) lxor (!bb land !c) in
      let t2 = (s0 + maj) land m in
      hh := !g;
      g := !f;
      f := !e;
      e := (!d + t1) land m;
      d := !c;
      c := !bb;
      bb := !a;
      a := (t1 + t2) land m
    done;
    h.(0) <- (h.(0) + !a) land m;
    h.(1) <- (h.(1) + !bb) land m;
    h.(2) <- (h.(2) + !c) land m;
    h.(3) <- (h.(3) + !d) land m;
    h.(4) <- (h.(4) + !e) land m;
    h.(5) <- (h.(5) + !f) land m;
    h.(6) <- (h.(6) + !g) land m;
    h.(7) <- (h.(7) + !hh) land m
  done;
  String.concat "" (Array.to_list (Array.map (Printf.sprintf "%08x") h))
