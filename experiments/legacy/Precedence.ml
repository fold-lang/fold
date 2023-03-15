
module Token = Lex.Token

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

(* XXX Add associativity *)
let lookup token =
  match Token.to_string token with
  (* Match atomic symbols. *)
  | "__eof__" -> Some delimiter
  | ";" -> Some 20
  | "!=" -> Some assignment
  (* Match symbols that can start an operator. *)
  | str ->
    begin match str.[0] with
      | '=' -> Some assignment
      | '>' -> Some conditional
      | '<' -> Some conditional
      | '#' | '&' -> Some conditional
      | '|'       -> Some (conditional + 1)
      | '+' | '-' -> Some sum
      | '*' | '/' -> Some product
      | ','       -> Some (group - 1)
      (* XXX These are not infix! *)
      (* | '(' | '{' | '[' -> Some group *)
      | _ -> None
    end

