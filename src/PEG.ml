
open Pure
open Lex




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


let rec to_parser self =
  let module P = Parser.Default in
  let (>>=) = P.(>>=) in
  let (<|>) = P.(<|>) in
  let (>>)  = P.(>>) in

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
    to_parser x

  | Alternative [x; y] ->
    to_parser x <|> to_parser y

  | Alternative (x::xs) ->
    List.fold_left (<|>) (to_parser x) (List.map to_parser xs)

  | Sequence [] ->
    P.pure []

  | Sequence [x] ->
    to_parser x

  | Sequence xs ->
    P.sequence (List.map to_parser xs) >>= fun xss -> P.pure (List.concat xss)

  | Optional x ->
    to_parser x <|> P.pure []

  | Many peg ->
    P.many (to_parser peg) >>= fun xss -> P.pure (List.concat xss)

  | Some peg ->
    P.some (to_parser peg) >>= fun xss -> P.pure (List.concat xss)



let rec to_pratt self : Syntax.expr list Pratt.Parser.t =
  let module PP = Pratt.Parser in
  let (>>=) = PP.(>>=) in
  let (<|>) = PP.(<|>) in
  let (>>)  = PP.(>>) in

  match self with
  | Epsilon ->
    PP.pure []

  | Terminal x ->
    PP.consume (Symbol x) >> lazy (PP.pure [])

  | Non_terminal _ ->
    Pratt.expression () >>= fun expr ->
    PP.pure [expr]

  | Alternative [] ->
    PP.pure []

  | Alternative [x] ->
    to_pratt x

  | Alternative [x; y] ->
    to_pratt x <|> to_pratt y

  | Alternative (x::xs) ->
    List.fold_left (<|>) (to_pratt x) (List.map to_pratt xs)

  | Sequence [] ->
    PP.pure []

  | Sequence [x] ->
    to_pratt x

  | Sequence xs ->
    PP.sequence (List.map to_pratt xs) >>= fun xss -> PP.pure (List.concat xss)

  | Optional x ->
    to_pratt x <|> PP.pure []

  | Many peg ->
    PP.many (to_pratt peg) >>= fun xss -> PP.pure (List.concat xss)

  | Some peg ->
    PP.some (to_pratt peg) >>= fun xss -> PP.pure (List.concat xss)


