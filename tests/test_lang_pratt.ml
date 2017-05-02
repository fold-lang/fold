
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
  Symbol "(", group (Symbol ")");
  Symbol ")", delimiter;
]


let atom = function
  | Int n -> singleton n
  | token -> Grammar.invalid_prefix token


let grammar =
  Grammar.init ~atom scope

let (=>) =
  test grammar


let () =
  "0"           => Ok 0;
  "-42"         => Ok (-42);
  "2 + 2"       => Ok 4;
  "2 + -2"      => Ok 0;
  "2 + 2 * 2"   => Ok 6;
  "(2 + 2) * 2" => Ok 8;

  "- - - - - 1" => Ok (-1);
  "(((((0)))))" => Ok 0;

  "x"           => Error "x cannot be used in prefix position";
  "()"          => Error "() cannot be used in prefix position";
  "2 +"         => Error "unexpected end of file";
  "2 1"         => Error "1 cannot be used in infix position";
  "2 * 2 0"     => Error "0 cannot be used in infix position";
  "/ 2"         => Error "/ cannot be used in prefix position";

