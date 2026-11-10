(** The executed instructions with the state each one updated: the registers that changed, the memory
    bytes written (with the byte they replaced when the capture showed it) and the I/O transfers. Only
    what the capture shows is known: the memory image is made of the bytes the bus carried so far (code
    fetched, data read or written), so a byte not seen before is reported as [?], never as zero. *)

type update =
  | Reg of { name : string; before : int option; after : int option }
  | Mem of { addr : int; before : int option; after : int }
  | In of { port : int; value : int }
  | Out of { port : int; value : int }

type step = { instr : Instr.t; updates : update list }

val of_events : Events.t list -> step list

val line : step -> string
(** One line: [pc], the instruction, then the updates in the order registers, memory, I/O. *)

val lines : step list -> string list

type final = { regs : Regs.t option; memory : (int * int) list; instructions : int }

val final : step list -> final
(** The registers after the last instruction that has them, and every memory byte the capture showed,
    by address. *)

val summary : final -> string list
(** The registers, then the memory as runs of consecutive addresses. *)
