(** CW1, QUESTION 3 **)

(* The type of (black-and-white) pictures *)
type pict = bool list list 

(* The type of descriptions of rows *)
type rdesc = int list

(* The type of complete (2D) nonogram descriptions *)
type nondesc = rdesc list * rdesc list

(* Helper function to get the length of a list *)
let rec length lst =
  match lst with
  | [] -> 0
  | _ :: tail -> 1 + length tail (* This function calculates the length of a list using recursion *)

(* a (3 marks) *)


(* Check if all rows in the picture have the same length *)
let rec validPict (p: pict) : bool =
  match p with
  | [] -> true  (* An empty picture is considered valid *)
  | first_row :: rest ->
      let row_length = length first_row in  (* Calculate the length of the first row *)
      let rec same_length lst =
        match lst with
        | [] -> true  (* All rows have been checked and have the same length *)
        | row :: tail -> (length row = row_length) && same_length tail
        (* Check if the length of the current row matches the first row and continue with the rest *)
      in
      same_length rest

(* Check if a nonogram description is valid *)
let rec validNondesc (nd: nondesc) : bool =
  let (row_descs, col_descs) = nd in
  (* Check if all row and column descriptions are non-negative *)
  let rec non_negative descs =
    match descs with
    | [] -> true
    | desc :: tail ->
        let rec all_non_negative lst =
          match lst with
          | [] -> true
          | h :: t -> (h >= 0) && all_non_negative t
          (* Check if each element in the list is non-negative *)
        in
        all_non_negative desc && non_negative tail
  in
  non_negative row_descs && non_negative col_descs

(* b (4 marks) *)

(* checkPict : nondesc -> pict -> bool *)
let checkPict = failwith "Not implemented"

(* c (5 marks) *)

(* nonSols1d : rdesc -> int -> bool list list *)
let nonSols1d = failwith "Not implemented"
