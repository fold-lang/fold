
open Pure
open Fold
open Fold.Lex
open Fold.Syntax

module C = Colors

module Pratt = Pratt.Make(Expr)
open Pratt


let show = function
  | Ok x -> Expr.to_string x
  | Error e -> e


(* Testing function *)

let test grammar input expected =
  let lexer = Lexer.from_string input in
  let actual = Pratt.parse ~grammar lexer in
  if actual = expected then
    print ("%s %s" % (C.bright_green "✓", C.bright_white input))
  else begin
    print ("%s %s" % (C.bright_red "✗", C.bright_white input));
    print ("  - Expected: %s" % C.green (show expected));
    print ("  - Actual:   %s" % C.red (show actual))
  end


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


let (=>) = test grammar

let () =
  "0"                                  => Ok (Atom (Int 0));
  "-42"                                => Ok (Form [Atom (Symbol "-"); Atom (Int 42)]);

  "f x"                                => Ok (f1 x);
  "f x y z"                            => Ok (f3 x y z);
  "f True 42 3.14"                     => Ok (f3 bT i42 f3_14);

  "(x)"                                => Ok x;
  "f (f x)"                            => Ok (f1 (f1 x));
  "f 42 (f (f (True) 3.14) x (f x y))" => Ok (f2 i42 (f3 (f2 bT f3_14) x (f2 x y)));
  "(((((0)))))"                        => Ok (Atom (Int 0))

