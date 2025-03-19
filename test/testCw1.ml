(* If you decide to write your own tests, you may want to uncomment the following line.

open QCheck
 *)

(* utility functions *)

(* Question 1 *)

module Q1 = Cw1.Question1

let _ = print_string ("================================================================================\n"
                    ^ "=== Testing Question 1 =========================================================\n")



let n1 = QCheck_runner.run_tests ~out:stdout
           [ (* add QCheck tests here *)
           ]

(* Question 2 *)

module Q2 = Cw1.Question2

let _ = print_string ("================================================================================\n"
                    ^ "=== Testing Question 2 =========================================================\n")

let n2 = QCheck_runner.run_tests ~out:stdout
           [ (* add QCheck tests here *)
           ]

(* Question 3*)

let _ = print_string ("================================================================================\n"
                    ^ "=== Testing Question 3 =========================================================\n")

module Q3 = Cw1.Question3

let n3 = QCheck_runner.run_tests ~out:stdout
           [ (* add QCheck tests here *)
           ]

(* error code is the number of failed/errored tests *)
let _ = print_int (n1 + n2 + n3); exit n1 + n2 + n3
