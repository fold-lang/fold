
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
  Grammar.init
    ~atom:(fun x -> singleton (Atom x))
    ~form:(fun x -> Lang.default_operator x or lazy (Lang.juxtaposition x))
    ()
  |> between "(" ")" id


let parse peg input =
  (* Grammar.dump grammar; *)
  let p = PEG.to_pratt peg in
  let s = State.init ~grammar ~lexer:(Lexer.from_string input) () in
  match Parser.run p s with
  | Ok (x, s) ->
    Ok x
  | Error e -> Error (Parser.error_to_string e)


let test peg input expected =
  Test.(test (result (list (module Expr)) string))
  input (parse peg input) expected


let () =
  let open PEG.DSL in
  let open Expr in

  let (=>) = test (seq [term "let"; expr "a"; term "="; expr "b"]) in
  Test.group "Let-expression" [
    "let x = 0" => Ok [symbol "x"; int 0];
    "let x = 2 + 2" => Ok [symbol "x"; form [symbol "+"; int 2; int 2]];
    "let" => Error "unexpected end of file";
    "let a" => Error "expected `=` but got `__eof__`";
    "let a =" => Error "unexpected end of file";
  ];

  let (=>) = test (seq [term "if"; expr "t"; term "then"; expr "a"; term "else"; expr "b"]) in
  Test.group "If-expression" [
    {|if True then
      'x'
    else
      'y'
    |} => Ok [bool true; char 'x'; char 'y'];

    {|if x - 1 == 0 then
      print "yes!"
    else
      print "no!"
    |} => Ok [form [symbol "=="; form [symbol "-"; symbol "x"; int 1]; int 0];
              form [symbol "print"; string "yes!"]; form [symbol "print"; string "no!"]];
  ];

  let (=>) = test (seq [term "while"; expr "t"; term "do"; expr "a"; term "end"]) in
  Test.group "While-expression" [
    {|while x / 2 != 0 do
      print "Hello, world!"
    end
    |} => Ok [form [symbol "!="; form [symbol "/"; symbol "x"; int 2]; int 0];
              form [symbol "print"; string "Hello, world!"]];
  ];

  let (=>) = test (seq [expr "name"; term "="; expr "value"])  in
  Test.group "As-pattern" [
    "val k = x + 1; no" => Ok []
  ]

