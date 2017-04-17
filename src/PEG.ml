
open Pure
open Lex

module P = Parser.Default


let (>>=) = P.(>>=)
let (<|>) = P.(<|>)
let (>>)  = P.(>>)


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


let rec parse self : 'a list P.t =
  match self with
  | Epsilon ->
    P.pure []

  | Terminal x ->
    P.expect (Symbol x) >>= fun tok ->
    P.advance >> lazy (P.pure [tok])

  | Non_terminal x ->
    fail "not implemented"

  | Alternative [] ->
    P.pure []

  | Alternative [x] ->
    parse x

  | Alternative [x; y] ->
    parse x <|> parse y

  | Alternative (x::xs) ->
    List.fold_left (<|>) (parse x) (List.map parse xs)

  | Sequence [] ->
    P.pure []

  | Sequence [x] ->
    parse x

  | Sequence xs ->
    P.sequence (List.map parse xs) >>= fun xss -> P.pure (List.concat xss)

  | Optional x ->
    parse x <|> P.pure []

  | Many peg ->
    P.many (parse peg) >>= fun xss -> P.pure (List.concat xss)

  | Some peg ->
    P.some (parse peg) >>= fun xss -> P.pure (List.concat xss)

