(** The architectural registers as the model compares them: [A], [B]..[L], the flag byte
    [S Z 0 AC 0 P 1 CY], [SP] and [PC]. A register the trace does not show is [None]. *)

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

val names : string list
val get : t -> string -> int option
val of_frame : Frame.regs -> t
val unknown : t

val of_ints :
  a:int -> b:int -> c:int -> d:int -> e:int -> h:int -> l:int -> f:int -> sp:int -> pc:int -> t

val changes : t -> t -> (string * int option * int option) list
(** The registers whose value differs, as [(name, before, after)]. *)

val flags_of : z:int -> s:int -> p:int -> c:int -> ac:int -> int
