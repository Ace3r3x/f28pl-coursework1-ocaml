(* This is an *interface* file, which specifies the types of top-level
   functions exported from its analogue for usage and testing: do not touch! *)

val shiftL : int -> 'a list -> 'a list

val shiftR : int -> 'a list -> 'a list

val isShiftedL : 'a list -> 'a list -> bool * int
