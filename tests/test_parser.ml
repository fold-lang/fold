
module P = Fold.Parser.Make(Fold.Syntax)
module L = Fold.Lex.Lexer
module E = Fold.Syntax.Expression
module T = Nanotest

let should msg input expected =
  let actual = P.run P.Expression.parser L.(to_stream (of_string input)) in
  T.test ~verbose:true (T.result (module E) (T.testable P.P.pp_error)) msg ~expected ~actual

let (==>) = (@@)


let a, b, c, d, x =
  `Symbol "a", `Symbol "b", `Symbol "c", `Symbol "d", `Symbol "x"

let f args =
  `Apply (`Symbol "f", args)

let (+) a b = `Apply (`Symbol "+", [a; b])

let tuple items = `Tuple items

let () =
  T.group "Units, tuples and expression groups" [
    should "parse unit value"
      "()" ==> Ok (E.token (`Symbol "()"));

    should "parse simple group"
      "(a)" ==> Ok (E.token a);

    should "parse compound group"
      "(f x + a)" ==> Ok (f [x] + a);

    should "parse simple pair"
      "(a, b)" ==> Ok (tuple [a; b]);

    should "parse simple triple"
      "(a, b, c)" ==> Ok (tuple [a; b; c]);

    should "parse simple quadruple"
      "(a, b, c, d)" ==> Ok (tuple [a; b; c; d]);
  ]

