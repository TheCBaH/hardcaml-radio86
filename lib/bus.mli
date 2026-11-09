(** The bus and control model: machine cycles, accepted transfers, waits and reset epochs, from the pins
    alone. A transfer is reported once, when the CPU accepts it (the sample that ends a read, the last
    low [WR̄] sample of a write, the first [WAIT] sample of a halt acknowledge), not at every strobe
    sample. Work that does not finish is reported as such. The model is a pure function of the frames. *)

type kind =
  | Fetch  (** Instruction fetch, status [a2]. *)
  | Mem_read  (** [82]. *)
  | Mem_write  (** [00]. *)
  | Stack_read  (** [86]. *)
  | Stack_write  (** [04]. *)
  | Io_read  (** [42]. *)
  | Io_write  (** [10]. *)
  | Int_ack  (** [23] and [2b]. *)
  | Halt_ack  (** [8a]. *)
  | Other  (** Any other status word: reported, not guessed. *)

val kind_name : kind -> string
val classify : int -> kind

val is_read : int -> bool
(** The status word's [WO̅] bit: a read-like cycle, with [DBIN]. *)

type cycle = {
  epoch : int;
  seq : int;  (** Cycle number within the capture. *)
  kind : kind;
  status : int option;
  addr : int option;  (** [A] in the [T1] sample (the port for I/O). *)
  data : int option;  (** The accepted or written byte; [None] for a halt acknowledge. *)
  first : int;  (** [T1]'s T-state number. *)
  last : int;  (** The T-state that accepted the transfer. *)
  waits : int;  (** Samples with [WAIT] high inside the cycle. *)
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
      (** The cycle ended without an accepted transfer: [data] is what was seen so far. *)
  | Note of { at : int; msg : string }  (** Unknown data or an unexpected pin combination. *)

type t

val init : t
val step : t -> Frame.t -> t * out list

val finish : t -> out list
(** End of the capture: an open cycle is reported as {!Abandoned}. *)
