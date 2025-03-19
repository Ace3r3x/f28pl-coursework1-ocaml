(* This is an *interface* file, which specifies the types of top-level
   functions exported from its analogue for usage and testing: do not touch! *)

type suit
type card

val validSuit : suit -> bool
val validCard : card -> bool

val parseSuit : string -> suit
val parseCard : string -> card

val winner : suit option -> card list -> int
