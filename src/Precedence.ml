
let delimiter   =  0
let keyword     =  1
let assignment  = 10
let conditional = 20
let sum         = 30
let product     = 40
let exponent    = 50
let prefix      = 60
let postfix     = 70
let call        = 80
let group       = 80
let terminal    = 90

let lookup name =
  match name with
  (* Match atomic symbols. *)
  | "<EOF>" -> delimiter
  | ";" -> 20
  (* Match symbols that can start an operator. *)
  | str ->
    begin match str.[0] with
      | '=' -> assignment
      | '#' -> conditional
      | '+' | '-' -> sum
      | '*' | '/' -> product
      | '(' | '{' | '[' -> group
      | _ -> 30 (* default non symbolic? XXX *)
    end
