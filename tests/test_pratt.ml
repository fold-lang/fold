
open Pure
open Fold
open Fold.Pratt
open Fold.Lex
open Fold.Syntax

module C = Colors

let show = Result.show Expr.pp Fmt.string


let (=>) input expected =
  let lexer = Lexer.from_string input in
  let actual = Pratt.parse lexer in
  if actual = expected then
    print ("%s %s" % (C.bright_green "✓", C.bright_white input))
  else begin
    print ("%s %s" % (C.bright_red "✗", C.bright_white input));
    print ("  - Expected: %s" % C.green (show expected));
    print ("  - Actual:   %s" % C.red (show actual))
  end



let f, g, x, y, z =
  let s = fun x -> Symbol x in
  s "f", s "g", s "x", s "y", s "z"

let i42, i0, i1, f3_14, bT =
  Int 42, Int 0, Int 1, Float 3.14, Bool true


let () =
  print "Testing Fold.Pratt...";
  "x" => Ok (Atom x);
  "42" => Ok (Atom i42);
  "3.14" => Ok (Atom f3_14);
  "True" => Ok (Atom bT)

