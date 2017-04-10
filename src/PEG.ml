
open Pure
open Syntax
open Lex

module Parser = struct
  include Pratt.Parser

  let rec many p =
    p >>= fun x  -> many p
      >>= fun xs -> pure (x :: xs)

end

let (>>=) = Parser.(>>=)
let (<|>) = Parser.(<|>)


type t =
  | Empty            (* ε     *)
  | Seq   of t list  (* a b   *)
  | Alt   of t * t   (* a | b *)
  | Opt   of t       (* a?    *)
  | Many  of t       (* a*    *)
  | Some  of t       (* a+    *)
  | Term  of string  (* "x"   *)
  | Expr  of string  (* a     *)



let rec seq =
  let rec go args list =
    match list with
    | [] -> args
    | x :: xs -> go (to_parser x :: args) xs
  in
    go []

and to_parser self : expr list Parser.t =
  match self with
  | Empty ->
    Parser.empty

  (* syntax seq ["kw1", expr1, "kw2", "kw2", expr2] => Ok [expr1, expr2] *)
  | Seq xs ->
    undefined ()

  | Alt (a, b) ->
    to_parser a <|> to_parser b

  | Opt a ->
    to_parser a <|> Parser.empty

  | Many a ->
    Parser.many (to_parser a) >>= (Expr.form >> Parser.pure) <|> Parser.empty

  | Some a ->
    Parser.many (to_parser a) >>= (Expr.form >> Parser.pure)

  | Term name ->
    Parser.expect (Symbol name) >>= fun x -> Parser.advance >>= fun () -> Parser.pure x

  | Expr name ->
    Parser.expression () >>= fun x -> Parser.advance >>= fun () -> Parser.pure x


let parse p =
  Parser.run_parser_with_string (to_parser p)


let test () =
  let a_or_b =
    Alt (Term "a", Term "b") in
  let _a_and_b =
    Seq (Term "a", Term "b") in
  let _many_a =
    Many (Term "a") in

  assert (parse a_or_b "a" = Some (Expr.symbol "a"));
  assert (parse a_or_b "a" = Some (Expr.symbol "b"))



let test_constructors () =
  (* syntax => Seq [] *)
  assert (parse (Seq []) "" = Error "empty");

  (* syntax "x" *)
  assert (parse (Term "x") "x" = None)



