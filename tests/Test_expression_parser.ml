
module P = Fold.Parser.Make(Fold.Syntax)
module L = Fold.Lex.Lexer

module Expr = Fold.Syntax.Expression
module Pat = Fold.Syntax.Pattern

open Nanotest

(* Local Testing DSL *)

let test msg input expected =
  let actual = P.run P.Expression.parser L.(to_stream (of_string input)) in
  let testable = result (module Expr) (testable (Fmt.of_to_string P.P.error_to_string)) in
  test ~verbose:true testable msg ~expected ~actual


(* Local Helper Definitions *)

let a, b, c, d, e =
  `Symbol "a", `Symbol "b", `Symbol "c", `Symbol "d", `Symbol "e"

let f args =
  `Apply (`Symbol "f", args)

let (+) a b = `Apply (`Symbol "+", [a; b])

let tuple items = `Tuple items


(* Tests *)

let () = begin
  group "Units, tuples and expression groups" [
    test "parse unit value"
      "()" ==> Ok (Expr.token (`Symbol "()"));

    test "parse simple group"
      "(a)" ==> Ok (Expr.token a);

    test "parse compound group"
      "(f a + b)" ==> Ok (f [a] + b);

    test "parse simple pair"
      "(a, b)" ==> Ok (tuple [a; b]);

    test "parse simple triple"
      "(a, b, c)" ==> Ok (tuple [a; b; c]);

    test "parse simple quadruple"
      "(a, b, c, d)" ==> Ok (tuple [a; b; c; d]);
  ];

  group "Invalid cases" [
    test "fail on incomplete unit value"
      "(" ==> Error (P.P.unexpected_end ());

    test "fail on unclosed tuple value 1"
      "(1," ==> Error (P.P.unexpected_token (`Symbol ",") ~expected:(`Symbol ")"));

    test "fail on unclosed tuple value 2"
      "(1,2" ==> Error (P.P.unexpected_end ~expected:(`Symbol ")") ());
  ];

  group "Simple bindings" [
    test "parse simple let-binding"
      "let a = 1 in a" ==> Ok (Expr.let' [a, `Int 1] a);

    test "parse two let-bindings"
      "let a = 2, b = 2 in a + b" ==> Ok (Expr.let' [a, `Int 2; b, `Int 2] (a + b));

    test "parse three let-bindings"
      "let a = 1, b = 2, c = 3 in a + b + c"
        ==> Ok (Expr.let' [a, `Int 1; b, `Int 2; c, `Int 3] (a + b + c));

    test "parse two nested let-bindings"
      "let a = 2 in let b = 2 in a + b"
        ==> Ok Expr.(let' [a, `Int 2] (let' [b, `Int 2] (a + b)));
  ];
end

