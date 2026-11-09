(** The contracts' pin-trace format (K4, [i8080-pin-trace v1]) as frames, and the comparison of the events
    of two captures. [~] cells are masked and [zz] cells are undriven: both are unknown ([None]), never
    zero. *)

val parse : string -> (Frame.t list, string) result
(** Rows are [n a do di sync dbin wr_n wait hlda inte ready hold int reset]; [n] counts from the first [T1]. *)

type diff = string

val compare_transfers : expected:Bus.cycle list -> actual:Bus.cycle list -> offset:int -> diff list
(** Kind, first and last T-state (the actual ones minus [offset]), waits, and the address and data where
    both are known. *)

val first_t1 : Events.t list -> int option
(** The T-state of the first cycle start, for [offset]. *)
