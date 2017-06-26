
module C = Colors

open Pure
open Base
open Fold
open Fold.Lex

module Pratt   = Pratt.Make(Syntax)
module Grammar = Pratt.Grammar

open Pratt


let grammar =
  Grammar.init
    ~atom:(fun x -> singleton (x :> Syntax.t))
    ~form:(fun x -> Lang.default_operator x or lazy (Lang.juxtaposition x))
    ()
  |> between "(" ")" id


let test grammar input expected =
  Test.(test (result (list (module Syntax)) string))
  input (run (Parser.many expression) ~grammar (Lexer.from_string input)) expected


let () =
  let open PEG.DSL in
  let (>>=) = Parser.(>>=) in

  let let_rule = PEG.to_pratt (seq [term "let"; expr "a"; term "="; expr "b"]) in
  let grammar = Grammar.define_prefix (`Symbol "let") (let_rule >>= fun xs -> Parser.pure (`Form xs)) grammar in

  let (=>) = test grammar in
  Test.group "Let-expression" [
    {|let x = 0
    let y = 1
    let z = x + y
    |} => Ok [`Form [`Symbol "x"; `Int 0]; `Form [`Symbol "y"; `Int 1];
              `Form [`Symbol "z"; `Form [`Symbol "+"; `Symbol "x"; `Symbol "y"]]];
  ];

  Test.group "Failing" [
    "print (2 + 3)" => Ok [`Form [`Symbol "pr`Int"; `Form [`Symbol "+"; `Int 2; `Int 3]]];
  ]

