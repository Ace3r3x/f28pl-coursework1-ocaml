(** CW2, QUESTION 2 **) 
(* a (3 marks) *)

(* Define the suit type *) 
(* Using a variant type ensures that only valid suits can be represented. *)
type suit = Clubs | Diamonds | Hearts | Spades;;

(* Define the card type *)
(* Using a variant type ensures that only valid card values can be represented. *)
type card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace;;

(* Function to check if a suit is valid *) 
(* This function simply returns true for all cases since invalid suits cannot exist. *) 
let validSuit (s:suit) : bool =  
  match s with 
  | Clubs | Diamonds | Hearts | Spades -> true;;

(* Function to check if a card is valid *)
(* The validCard function checks if the input matches one of the defined card ranks, ensuring that it is a correct representation of a card. *)
let validCard (c:card) : bool =  
  match c with 
  | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace -> true;;


(* b (3 marks) *)
(* Function to parse a suit from a single-character string *)
(* The first character represents the card value and the second represents the suit. *)
(* This function checks the length of the input and matches against valid characters. *)
let parseCard (su: string) : card * suit =  
  if String.length su <> 2 then
    failwith "Invalid card: should be a two-character string" (* Ensure valid input length *)
  else
    (* Invalid value check *)
    let card = 
      match su.[0] with 
      | '2' -> Two | '3' -> Three | '4' -> Four | '5' -> Five | '6' -> Six | '7' -> Seven | '8' -> Eight | '9' -> Nine | 'T' -> Ten | 'J' -> Jack | 'Q' -> Queen | 'K' -> King | 'A' -> Ace 
      | _ -> failwith "Invalid card value: should be one of 2-9, T, J, Q, K, A" (* Invalid value check *)
    in 
(* Parse the suit by matching the second character. *)
    let suit = 
      match su.[1] with 
      | 'C' -> Clubs | 'D' -> Diamonds | 'H' -> Hearts | 'S' -> Spades 
      | _ -> failwith "Invalid suit: should be one of C, D, H, S" (* Invalid suit check *) 
    in
    (card, suit);;


(* c (4 marks) *)
(* Function to calculate the value of a card *)
(* Each card is assigned its standard value, with Ace being the highest. *)
let cardValue (c: card) : int =  
  match c with
  | Two -> 2 | Three -> 3 | Four -> 4 | Five -> 5 | Six -> 6 | Seven -> 7 | Eight -> 8 | Nine -> 9 | Ten -> 10 | Jack -> 11 | Queen -> 12 | King -> 13 | Ace -> 14 ;; (* Ace has the highest value *)
                                                                                                                                                                      
(* Helper function to get the head of a list, raising an error if the list is empty. *)
let hd lst = 
  match lst with
  | [] -> failwith "Empty list" (* Empty list is not valid for this function *)
  | h::_ -> h

(* Recursive function to filter elements from a list that satisfy a predicate. *)
let rec filter pred lst =
  match lst with
  | [] -> [] (* Base case: Empty list returns empty *)
  | h::t -> if pred h then h :: (filter pred t) else filter pred t (* Recursive case: Keep elements that match the predicate *) 

(* Custom fold_left function *)
(* Recursive function to fold (reduce) a list using an accumulator and a function. *)
let rec fold_left f acc lst =
  match lst with
  | [] -> acc (* Base case: Return the accumulated result when the list is empty *)
  | h::t -> fold_left f (f acc h) t (* Recursive case: Apply the function to the accumulator and the head of the list *)

(* Function to find the index of an element *)
let rec index_of x lst =
  let rec aux i = function
    | [] -> -1 (* Base case: Return -1 if the element is not found *)
    | h::t -> if h = x then i else aux (i + 1) t (* Recursive case: Check each element until a match is found *)
  in aux 0 lst

(* Function to determine the winning card *)
(* The trump suit is given as an option type, and the cards are a list of card-suit pairs. *)
let winner (trump: suit option) (cards: (card * suit) list) : int =
  let leadSuit = snd (hd cards) in (* The suit of the first card is the lead suit *)
  let trumpCards = filter (fun (_, s) -> Some s = trump) cards in (* Collect all trump cards if a trump suit exists *)
  let leadCards = filter (fun (_, s) -> s = leadSuit) cards in (* Collect all cards that follow the lead suit *)
  let bestCard (bestIndex, bestValue) (card, suit) =
    let value = cardValue card in
        (* If the card matches the trump or lead suit and has a higher value, update the best card *) 
    if (Some suit = trump || suit = leadSuit) && value > bestValue then
      (index_of (card, suit) cards, value)
    else
      (bestIndex, bestValue)
  in
  (* If trump cards are present, find the highest trump card. Otherwise, find the highest lead suit card. *)
  if trumpCards <> [] then
    (* Find the highest trump card *)
    let (winningIndex, _) = fold_left bestCard (-1, 0) trumpCards in
    winningIndex
  else
    (* No trump cards, find the highest lead card *)
    let (winningIndex, _) = fold_left bestCard (-1, 0) leadCards in
    winningIndex
      