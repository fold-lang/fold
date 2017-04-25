
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
  | "__eof__" -> Some delimiter
  | ";" -> Some 20
  (* Match symbols that can start an operator. *)
  | str ->
    begin match str.[0] with
      | '=' -> Some assignment
      | '#' -> Some conditional
      | '+' | '-' -> Some sum
      | '*' | '/' -> Some product
      | '(' | '{' | '[' -> Some group
      | _ -> None
    end

