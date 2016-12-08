
open Pure
open Fold.Lex

let () =
  let expected =
    Token.(Location.{ line = 0; column = 1; length = 3 },
           Literal.Symbol "hey") in
  let lexer = Lexer.from_string "hey" in
  let token = Lexer.next lexer in
  assert (token = expected)

