
open Pure

let (>>=) = Parser.(>>=)
let (>>) = Parser.(>>)


type t =
  | Epsilon                 (* ε         *)
  | Terminal     of string  (* "x"       *)
  | Non_terminal of string  (* a         *)
  | Sequence     of t list  (* a b ...   *)
  | Alternative  of t list  (* a | b ... *)
  | Optional     of t       (* a?        *)
  | Many         of t       (* a*        *)
  | Some         of t       (* a+        *)


let rec to_string self =
  match self with
  | Epsilon         -> "ε"
  | Terminal     x  -> "\"" ^ x ^ "\""
  | Non_terminal x  -> x
  | Sequence     xs -> "(" ^ String.concat " "   (List.map to_string xs) ^ ")"
  | Alternative  xs -> "(" ^ String.concat " | " (List.map to_string xs) ^ ")"
  | Optional     x  -> to_string x ^ "?"
  | Many         x  -> to_string x ^ "*"
  | Some         x  -> to_string x ^ "+"


let parse self =
  match self with
  | Epsilon ->
    Parser.pure []

  | Terminal x ->
    Parser.expect (Lex.Symbol x) >>= fun tok ->
    Parser.advance >> lazy (Parser.pure [tok])

  | _ -> Parser.(error Empty)


