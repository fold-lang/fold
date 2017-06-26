
open Pure
open Fold
open Fold.Lex

module C = Colors

module Pratt = Pratt.Make(Syntax)
open Pratt


(* Grammar definition *)

let juxtaposition tok =
  let precedence = 90 in
  let parse x =
    Pratt.parse_prefix precedence >>= fun y ->
    let list =
      match x with
      | `Form xs -> List.append xs [y]
      | atom    -> [atom; y] in
    Parser.pure (`Form list) in
  (parse, precedence)


let grammar =
  Grammar.init
    ~atom:(fun x -> singleton (x :> Syntax.t))
    ~form:juxtaposition
    ()
  |> between "(" ")" id



(* Some helper definitions *)

let x, y, z =
  let s = fun x -> `Symbol x in
  s "x", s "y", s "z"

let i42, i0, i1, f3_14, bT =
  `Int 42, `Int 0, `Int 1, `Float 3.14, `Bool true

let f1, f2, f3 =
  (fun x     -> `Form [`Symbol "f"; x]),
  (fun x y   -> `Form [`Symbol "f"; x; y]),
  (fun x y z -> `Form [`Symbol "f"; x; y; z])


let (=>) input expected =
  Test.(test (result (list (module Syntax)) string))
  input (run (Parser.many expression) ~grammar (Lexer.from_string input)) expected

let () =
  Test.group "Empty" [
    ""                                   => Ok [];
    " "                                  => Ok [];
    "\n"                                 => Ok [];
  ];

  Test.group "Atoms" [
    "0"                                  => Ok [`Int 0];
    "x"                                  => Ok [`Symbol "x"];
    "True"                               => Ok [`Bool true];
  ];

  Test.group "Forms" [
    "-42"                                => Ok [`Form [`Symbol "-"; `Int 42]];
    "f x"                                => Ok [f1 x];
    "f x y z"                            => Ok [f3 x y z];
    "f True 42 3.14"                     => Ok [f3 bT i42 f3_14];
    "f 42 (f (f (True) 3.14) x (f x y))" => Ok [f2 i42 (f3 (f2 bT f3_14) x (f2 x y))];
  ];

  Test.group "Weird" [
    "(x)"                                => Ok [x];
    "f (f x)"                            => Ok [f1 (f1 x)];
    "(((((0)))))"                        => Ok [`Int 0];
  ]

