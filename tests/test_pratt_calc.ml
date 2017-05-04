
open Pure

open Fold
open Lex
module C = Colors
module Pratt = Pratt.Make(Int)
open Pratt

module Int = struct
  include Int
  let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)
end

let rec fact x =
  if x <= 1 then 1 else x * fact (x - 1)

let scope = Rule.[
  Symbol "+", prefix     (fun x -> +x);
  Symbol "+", infix   30 (fun x y -> x + y);
  Symbol "-", prefix     (fun x -> -x);
  Symbol "-", infix   30 (fun x y -> x - y);
  Symbol "*", infix   40 (fun x y -> x * y);
  Symbol "/", infix   40 (fun x y -> x / y);
  Symbol "^", infixr  50 Int.pow;
  Symbol "!", postfix 70 (fun x -> fact x);
  Symbol "(", group (Symbol ")");
  Symbol ")", delimiter;
]

let atom = function
  | Int n -> Rule.singleton n
  | token -> Grammar.invalid_prefix token


let grammar = Grammar.init ~atom scope

let (=>) input expected =
  Test.(test (result (module Int) string))
  input (Pratt.(run expression) ~grammar (Lexer.from_string input)) expected

let () =
  Test.group "Math" [
    "0"            => Ok 0;
    "-42"          => Ok (-42);
    "2 + 2"        => Ok 4;
    "2 + -2"       => Ok 0;
    "2 + 2 * 2"    => Ok 6;
    "(2 + 2) * 2"  => Ok 8;
  ];

  Test.group "Postfix" [
    "5!"           => Ok 120;
    "5! + 1"       => Ok 121;
    "-5!"          => Ok (-120);
    "(2 + 2) * 2!" => Ok 8;
    "(2 + 2) * !2" => Error "! cannot be used in prefix position";
  ];

  Test.group "Infix right" [
    "(2 ^ 2) ^ 3"  => Ok 64;
    "2 ^ (2 ^ 3)"  => Ok 256;
    "2 ^ 2 ^ 3"    => Ok 256;
  ];

  Test.group "Edge cases" [
    "- - - - - 1"  => Ok (-1);
    "(((((0)))))"  => Ok 0;
  ];

  Test.group "Errors" [
    "x"            => Error "x cannot be used in prefix position";
    "()"           => Error "() cannot be used in prefix position";
    "2 +"          => Error "unexpected end of file";
    "2 1"          => Error "1 cannot be used in infix position";
    "2 * 2 0"      => Error "0 cannot be used in infix position";
    "/ 2"          => Error "/ cannot be used in prefix position";
  ]

