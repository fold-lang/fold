
open Pure

open Fold
open Lex
module C = Colors
module Pratt = Pratt.Make(Int)
open Pratt


let show = function
  | Ok x -> Int.to_string x
  | Error e -> e


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


let scope = [
  Symbol "+", unary_prefix    (fun x -> +x);
  Symbol "+", binary_infix 30 (fun x y -> x + y);
  Symbol "-", unary_prefix    (fun x -> -x);
  Symbol "-", binary_infix 30 (fun x y -> x - y);
  Symbol "+", binary_infix 30 (fun x y -> x + y);
  Symbol "*", binary_infix 40 (fun x y -> x * y);
  Symbol "/", binary_infix 40 (fun x y -> x / y);
  Symbol "(", group (Symbol ")")
]

let number =
  atom (function
  | Int n -> n
  | token -> fail ("unsupported atom: %s" % (Token.to_string token)))


let grammar =
  Grammar.init ~atom:number scope

let (=>) =
  test grammar


let () =
  "0"           => Ok 0;
  "-42"         => Ok (-42);
  "2 + 2"       => Ok 4;
  "2 + -2"      => Ok 0;
  "2 + 2 * 2"   => Ok 6;
  "(2 + 2) * 2" => Ok 8









