(** Lockstep against Rivet's behavioural model of the 8080 (A9): the observed instructions are replayed
    one by one in the model, and the program counter, the fetched bytes, the ordered data accesses, the
    registers after the instruction and the T-state count are compared. The model gets the same program,
    the same start state and, for [IN], the byte the bus delivered. The first difference is reported with
    the T-states and the waveform times that contain it. *)

type setup = {
  memory : (int * string) list;
      (** Loaded into the model's 64 KiB before the run: (address, bytes). *)
  pc : int;  (** The address of the first instruction compared. *)
  sp : int;
  known : (int * string * string) list;
      (** Classified differences: (first opcode byte, kind, note). A difference of that kind in that
          instruction is recorded in {!report.classified} and does not stop the run. *)
  start_regs : Regs.t option;
      (** The registers of the model at [pc]; unset ones are zero, flags [02] (the harness's boot state). *)
}

type plant =
  | No_plant
  | Ult
  | Aux
  | Carry  (** Deliberate model bugs, to show that the check notices one. *)

type divergence = {
  idx : int;  (** The observed instruction's index. *)
  pc : int option;
  what : string;
      (** [pc], [bytes], [accesses], [access order], [reg <name>], [tstates], [capture] or [unobserved]. *)
  detail : string;
  first : int;
  last : int;  (** T-states of the observed instruction. *)
  time_first : int64 option;
  time_last : int64 option;  (** Waveform timestamps of those samples. *)
}

type report = {
  compared : int;  (** Instructions in agreement. *)
  skipped : int;  (** Observed before [pc]: the harness's boot preamble. *)
  partial : int;
      (** Compared without registers (the last instruction, whose successor never began). *)
  tail : int;  (** A last fetch the capture cut short, left out. *)
  halted : bool;  (** The model halted where the observation did. *)
  divergence : divergence option;  (** The first. *)
  model_tstates : int;
  all : divergence list;  (** With [~all], every one found; otherwise just the first. *)
  classified : (divergence * string) list;  (** The known differences met, with their notes. *)
}

val run :
  ?plant:plant -> ?all:bool -> setup -> time_of:(int -> int64 option) -> Instr.t list -> report

val plant_of_string : string -> plant option
val pp_divergence : Format.formatter -> divergence -> unit
