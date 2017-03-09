
open Syntax

module Parser = Pratt.Parser

let (>>=) = Parser.(>>=)
let (<|>) = Parser.(<|>)


type t =
  | Empty          (* ε     *)
  | Seq of t * t   (* a b   *)
  | Alt of t * t   (* a | b *)
  | Opt of t       (* a?    *)
  | Many0 of t     (* a*    *)
  | Many1 of t     (* a+    *)
  | Term of string (* "x"   *)


let rec to_parser self =
  match self with
  | Empty ->
    Parser.empty

  | Seq (a, b) ->
    to_parser a >>= fun expr_1 ->
    to_parser b >>= fun expr_2 ->
    Parser.pure (Form [expr_1; expr_2])

  | Alt (a, b) ->
    to_parser a <|> to_parser b

  | Opt a ->
    to_parser a <|> Parser.empty

  | Many0 a ->
    let rec many p =
      p >>= fun x  -> many p
        >>= fun xs -> Parser.pure (x :: xs) in
    (many (to_parser a) >>= fun xs -> Parser.pure (Form xs))
    <|> Parser.pure (Form [])


