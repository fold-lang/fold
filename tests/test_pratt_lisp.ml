
open Pure
open Fold
open Fold.Lex
open Fold.Syntax

module C = Colors

module Pratt = Pratt.Make(Expr)
open Pratt


(* Grammar definition *)

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
  Grammar.init [
    Symbol "(", group (Symbol ")");
    Symbol ")", delimiter;
  ]
  ~atom:(fun x -> singleton (Atom x))
  ~form:juxtaposition


(* Some helper definitions *)

let x, y, z =
  let s = fun x -> Atom (Symbol x) in
  s "x", s "y", s "z"

let i42, i0, i1, f3_14, bT =
  let a = fun x -> Atom x in
  a (Int 42), a (Int 0), a (Int 1), a (Float 3.14), a (Bool true)

let f1, f2, f3 =
  (fun x     -> Form [Atom (Symbol "f"); x]),
  (fun x y   -> Form [Atom (Symbol "f"); x; y]),
  (fun x y z -> Form [Atom (Symbol "f"); x; y; z])


let (=>) input expected =
  Test.(test (result (list (module Expr)) string))
  input (Pratt.parse ~grammar (Lexer.from_string input)) expected

let () =
  Test.group "Atoms" [
    "0"                                  => Ok [Atom (Int 0)];
    "x"                                  => Ok [Atom (Symbol "x")];
    "True"                               => Ok [Atom (Bool true)];
  ];

  Test.group "Forms" [
    "-42"                                => Ok [Form [Atom (Symbol "-"); Atom (Int 42)]];
    "f x"                                => Ok [f1 x];
    "f x y z"                            => Ok [f3 x y z];
    "f True 42 3.14"                     => Ok [f3 bT i42 f3_14];
    "f 42 (f (f (True) 3.14) x (f x y))" => Ok [f2 i42 (f3 (f2 bT f3_14) x (f2 x y))];
  ];

  Test.group "Weird" [
    "(x)"                                => Ok [x];
    "f (f x)"                            => Ok [f1 (f1 x)];
    "(((((0)))))"                        => Ok [Atom (Int 0)];
  ]

