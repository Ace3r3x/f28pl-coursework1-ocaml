(** Question 1 **)
(* a *)
(* Helper function is used to calculate the length of a list *)
let rec length xs =
  match xs with
  | [] -> 0
  | _::t -> 1 + length t

(* Using helper function to concatenate two lists *)
let rec concat lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | h::t -> h :: (concat t lst2)

(* shiftL : int -> 'a list -> 'a list *)
(* Shifting a list to the left by n positions *)
let shiftL n xs =
  let len = length xs in
  if len = 0 then xs (* If the list is empty, it will return as it is *)
  else
    let n = n mod len in (* Calculateing effective shift count *)
    let rec take n lst =
      match lst with
      | [] -> ([], []) (* Returning an empty lists, if the input list is empty *)
      | h::t when n > 0 -> 
          let (taken, rest) = take (n - 1) t in
          (h::taken, rest) (* Spliting the list into two parts at nth position *)
      | _ -> ([], lst)
    in
    let (left_part, right_part) = take n xs in
    concat right_part left_part (* Concatenating right part to left part *)

(* shiftR : int -> 'a list -> 'a list *)
(* Shifting a list to the right by n positions *)
let shiftR n xs =
  let len = length xs in
  if len = 0 then xs (* If the list is empty, it will return as it is *)
  else
    let n = n mod len in (* Calculate effective shift count *)
    shiftL (len - n) xs;;  (* Shifting left by len - n to achieve right shift *)       
                       
                                       
(* b *)
(* isShiftedL : 'a list -> 'a list -> bool * int *)
(* Calculating the Length of a List by using helper function*)
let rec length xs =
  match xs with
  | [] -> 0 (* Base case: empty list has length 0 *)
  | _::t -> 1 + length t (* Recursive case: count the head and recurse on the tail *) 

(* Helper function is used to concatenate Two Lists *)
let rec concat lst1 lst2 =
  match lst1 with
  | [] -> lst2 (* Base case: if the first list is empty, return the second list *)
  | h::t -> h :: (concat t lst2) (* Recursive case: add head of lst1 to result of concatenating the tail of lst1 with lst2 *)

(* Checking for Left Shift, if ys is a left shift of xs *)
let isShiftedL xs ys =
  let len_xs = length xs in (* Calculating the length of the first list *)
  let len_ys = length ys in (* Calculating the length of the second list *)
  if len_xs <> len_ys then (false, -1)  (* If the lengths are not equal, ys cannot be a shift of xs *)

  else
    
    (* Using helper fucntion to take the first n elements and the rest of the list *)
    let rec shift_and_compare n xs ys =
      if n >= len_xs then (false, -1)  (* If all possible shifts have been tried, then return as false *)
      else
        let rec take n lst =
          match lst with
          | [] -> ([], []) (* Base case: empty list returns two empty lists *)
          | h::t when n > 0 -> 
              let (taken, rest) = take (n - 1) t in
              (h::taken, rest) (* Recursive case: add head to taken and recurse on the tail *)
          | _ -> ([], lst) (* When n is 0, return the current element as the start of the remaining list *)
        in
        let (left_part, right_part) = take n xs in (* Spliting the list at the nth position *)
        let shifted = concat right_part left_part in (* Concatenating right part followed by left part *)
        if shifted = ys then (true, n) (* If the shifted list matches ys, then return as true with the shift count *)
        else shift_and_compare (n + 1) xs ys  (* Otherwise, increment n and try the next shift *)
    in
    shift_and_compare 0 xs ys ;;  (* Start the recursive shifting process from n = 0 *)
