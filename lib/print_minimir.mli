open Minimir

val print : mir_body -> unit

(* Pour le debug dans le backend *)
val pp_instr : Format.formatter -> instr -> unit
