
module C = Colors

open Pure
open Fold
open Fold.Lex
open Fold.Syntax

module Pratt   = Pratt.Make(Expr)
module Grammar = Pratt.Grammar

open Pratt


let juxtaposition tok =
  let precedence = 90 in
  let parse x =
    Pratt.prefix precedence >>= fun y ->
    let list =
      match x with
      | Form xs -> List.append xs [y]
      | atom    -> [atom; y] in
    Parser.pure (Form list) in
  (parse, precedence)


let grammar =
  let open Rule in
  let open Syntax.Expr in
  Grammar.init [
    Symbol "==", infix   10 (fun x y -> form [symbol "=="; x; y]);
    Symbol "!=", infix   10 (fun x y -> form [symbol "!="; x; y]);
    Symbol "+",  prefix     (fun x   -> form [symbol "+"; x]);
    Symbol "+",  infix   30 (fun x y -> form [symbol "+"; x; y]);
    Symbol "-",  prefix     (fun x   -> form [symbol "+"; x]);
    Symbol "-",  infix   30 (fun x y -> form [symbol "-"; x; y]);
    Symbol "*",  infix   40 (fun x y -> form [symbol "*"; x; y]);
    Symbol "/",  infix   40 (fun x y -> form [symbol "/"; x; y]);
    Symbol "(",  group (Symbol ")");
    Symbol ")",  delimiter;
  ]
  ~atom:(fun x -> singleton (Atom x))
  ~form:juxtaposition


let parse peg input =
  let p = PEG.to_pratt peg in
  Pratt.run p ~grammar (Lexer.from_string input)


let test peg input expected =
  Test.(test (result (list (module Expr)) string))
  input (parse peg input) expected


let () =
  let open PEG.DSL in
  let open Expr in

  let (=>) = test (seq [term "let"; expr "a"; term "="; expr "b"]) in
  Test.group "Lex-expression" [
    "let x = 0" => Ok [symbol "x"; int 0];
    "let x = 2 + 2" => Ok [symbol "x"; form [symbol "+"; int 2; int 2]];
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

  let (=>) = test (seq [expr "patt"; term "as"; expr "name"])  in
  Test.group "As-pattern" [
    "True + 1 as flag" => Ok []
  ]





