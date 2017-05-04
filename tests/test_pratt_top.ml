
module C = Colors

open Pure
open Base
open Fold
open Fold.Lex
open Fold.Syntax

module Pratt   = Pratt.Make(Expr)
module Grammar = Pratt.Grammar

open Pratt


let grammar =
  let open Rule in
  let open Syntax.Expr in
  Grammar.init [
    Symbol "(",  group (Symbol ")");
    Symbol ")",  delimiter;
  ]
  ~atom:(fun x -> singleton (Atom x))
  ~form:(fun x -> Lang.default_operator x or lazy (Lang.juxtaposition x))


let test grammar input expected =
  Test.(test (result (list (module Expr)) string))
  input (Pratt.parse ~grammar (Lexer.from_string input)) expected


let () =
  let open PEG.DSL in
  let open Expr in
  let (>>=) = Parser.(>>=) in

  let let_rule = PEG.to_pratt (seq [term "let"; expr "a"; term "="; expr "b"]) in
  let grammar = Grammar.define (Symbol "let") (Grammar.Prefix (let_rule >>= fun xs -> Parser.pure (Form xs))) grammar in

  let (=>) = test grammar in
  Test.group "Let-expression" [
    {|let x = 0
    let y = 1
    let z = x + y
    |} => Ok [form [symbol "x"; int 0]; form [symbol "y"; int 1];
              form [symbol "z"; form [symbol "+"; symbol "x"; symbol "y"]]];
  ]

