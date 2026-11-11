(* Reference answers in plain OCaml, independent of the 8080 programs and of any CPU model: what
   a workload must leave in RAM is computed here, from its inputs, with host integer arithmetic.
   Authored in this repository (MIT). *)

(* {1 Ackermann} *)

(* The definition, evaluated with an explicit stack of pending first arguments so that deep
   recursion costs no host stack. *)
let ackermann m n =
  let rec go stack n =
    match stack with
    | [] -> n
    | 0 :: rest -> go rest (n + 1)
    | m :: rest -> if n = 0 then go ((m - 1) :: rest) 1 else go (m :: (m - 1) :: rest) (n - 1)
  in
  if m < 0 || n < 0 then invalid_arg "Oracle.ackermann";
  (* each step either lowers m, or lowers n while pushing an m; the host int is wide enough
     for every argument pair the workloads use *)
  go [ m ] n

(* The closed forms for m <= 3, a second way to the same numbers. *)
let ackermann_closed m n =
  match m with
  | 0 -> n + 1
  | 1 -> n + 2
  | 2 -> (2 * n) + 3
  | 3 -> (1 lsl (n + 3)) - 3
  | _ -> invalid_arg "Oracle.ackermann_closed: m > 3"

(* {1 Integer multiply and divide} *)

let word x = x land 0xffff

(* The low 16 bits of the product, in Int64 arithmetic. *)
let mul16 a b = Int64.(to_int (logand (mul (of_int (word a)) (of_int (word b))) 0xffffL))

(* hi:lo / d with hi < d and d <> 0: the 16-bit quotient and remainder. *)
let div32x16 ~hi ~lo ~d =
  let hi = word hi and lo = word lo and d = word d in
  if d = 0 || hi >= d then invalid_arg "Oracle.div32x16: outside the routine's contract";
  let dividend = Int64.(add (shift_left (of_int hi) 16) (of_int lo)) in
  let q = Int64.(to_int (div dividend (of_int d)))
  and r = Int64.(to_int (rem dividend (of_int d))) in
  (q, r)

(* The division identity, checked on the result rather than assumed: dividend = q * d + r with
   r < d and q within 16 bits. *)
let div_consistent ~hi ~lo ~d ~q ~r =
  let dividend = Int64.(add (shift_left (of_int (word hi)) 16) (of_int (word lo))) in
  r >= 0
  && r < word d
  && q >= 0 && q <= 0xffff
  && Int64.equal dividend Int64.(add (mul (of_int q) (of_int (word d))) (of_int r))

(* {1 Digits of pi}

   Machin's formula, pi = 16 atan(1/5) - 4 atan(1/239), on fixed-point numbers held as arrays of
   base-10000 limbs: a different algorithm from the spigot the 8080 program runs. The first
   limb is the integer part. *)

let base = 10000

let pi_digits n =
  if n < 1 then invalid_arg "Oracle.pi_digits";
  let limbs = ((n + 3) / 4) + 3 in
  (* one integer limb, [limbs] fractional limbs, three guard limbs among them *)
  let make v = Array.init (limbs + 1) (fun i -> if i = 0 then v else 0) in
  let is_zero a = Array.for_all (fun x -> x = 0) a in
  let div_small a d =
    let r = ref 0 in
    Array.map
      (fun x ->
        let cur = (!r * base) + x in
        r := cur mod d;
        cur / d)
      a
  in
  let add a b =
    let c = ref 0 in
    let out = Array.make (Array.length a) 0 in
    for i = Array.length a - 1 downto 0 do
      let s = a.(i) + b.(i) + !c in
      out.(i) <- s mod base;
      c := s / base
    done;
    out
  in
  let sub a b =
    let c = ref 0 in
    let out = Array.make (Array.length a) 0 in
    for i = Array.length a - 1 downto 0 do
      let s = a.(i) - b.(i) - !c in
      if s < 0 then (
        out.(i) <- s + base;
        c := 1)
      else (
        out.(i) <- s;
        c := 0)
    done;
    out
  in
  (* atan(1/x) = sum (-1)^k / ((2k+1) x^(2k+1)) *)
  let atan_inv x =
    let power = ref (div_small (make 1) x) in
    let sum = ref !power in
    let k = ref 1 and positive = ref false in
    while not (is_zero !power) do
      power := div_small !power (x * x);
      let term = div_small !power ((2 * !k) + 1) in
      sum := if !positive then add !sum term else sub !sum term;
      positive := not !positive;
      incr k
    done;
    !sum
  in
  let mul_small a m =
    let c = ref 0 in
    let out = Array.make (Array.length a) 0 in
    for i = Array.length a - 1 downto 0 do
      let s = (a.(i) * m) + !c in
      out.(i) <- s mod base;
      c := s / base
    done;
    out
  in
  let pi = sub (mul_small (atan_inv 5) 16) (mul_small (atan_inv 239) 4) in
  let buf = Buffer.create (4 * limbs) in
  Array.iteri
    (fun i x -> Buffer.add_string buf (if i = 0 then string_of_int x else Printf.sprintf "%04d" x))
    pi;
  String.sub (Buffer.contents buf) 0 n

(* The first 32 significant digits of pi, written down. The oracle above is checked against them. *)
let pi_32 = "31415926535897932384626433832795"
