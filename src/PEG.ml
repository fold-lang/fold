
open Pure
open Syntax

module Parser = struct
  include Pratt.Parser

  let rec many1 p =
    p >>= fun x  -> many1 p
      >>= fun xs -> pure (x :: xs)

end

let (>>=) = Parser.(>>=)
let (<|>) = Parser.(<|>)


type t =
  | Empty                   (* ε     *)
  | Sequence     of t * t   (* a b   *)
  | Alternative  of t * t   (* a | b *)
  | Optional     of t       (* a?    *)
  | Many0        of t       (* a*    *)
  | Many1        of t       (* a+    *)
  | Terminal     of string  (* "x"   *)
  | Non_terminal of string  (* a     *)



let rec to_parser self =
  match self with
  | Empty ->
    Parser.empty

  | Sequence (a, b) ->
    to_parser a >>= fun expr_1 ->
    to_parser b >>= fun expr_2 ->
    Parser.pure (Form [expr_1; expr_2])

  | Alternative (a, b) ->
    to_parser a <|> to_parser b

  | Optional a ->
    to_parser a <|> Parser.empty

  | Many0 a ->
    Parser.many1 (to_parser a) >>= (Expr.form >> Parser.pure) <|> Parser.pure (Form [])

  | Many1 a ->
    Parser.many1 (to_parser a) >>= (Expr.form >> Parser.pure)

  | Terminal name ->
    Parser.expect (Lex.Symbol name) >>= fun x -> Parser.advance >>= fun () -> Parser.pure x

  | Non_terminal name ->
    Parser.expression () >>= fun x -> Parser.advance >>= fun () -> Parser.pure x


