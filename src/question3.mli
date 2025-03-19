(* This is an *interface* file, which specifies the types of top-level
   functions exported from its analogue for usage and testing: do not touch! *)

(* The type of (black-and-white) pictures *)
type pict = bool list list 

(* The type of descriptions of rows *)
type rdesc = int list

(* The type of complete (2D) nonogram descriptions *)
type nondesc = rdesc list * rdesc list

val validPict : pict -> bool

val validNondesc : nondesc -> bool

val checkPict : nondesc -> pict -> bool

val nonSols1d : rdesc -> int -> bool list list
