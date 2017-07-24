
open Pure
open Fold

module Pratt = Pratt.Make(Int)
open Pratt

let rec fact x =
  if x <= 1 then 1 else x * fact (x - 1)

let atom = function
  | `Int n -> singleton n
  | token -> Grammar.invalid_prefix token

let grammar =
  Grammar.init ~atom ()
  |> prefix     "+"     (fun x -> +x)
  |> infix   30 "+"     (fun x y -> x + y)
  |> prefix     "-"     (fun x -> -x)
  |> infix   30 "-"     (fun x y -> x - y)
  |> infix   40 "*"     (fun x y -> x * y)
  |> infix   40 "/"     (fun x y -> x / y)
  |> infixr  50 "^"     (fun x y -> x ** y)
  |> postfix 70 "!"     (fun x -> fact x)
  |> between    "(" ")" (fun x -> x)


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

