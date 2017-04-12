
open Pure
open Fold.Lex
open Fold.Syntax

module P = Fold.Parser
module C = Colors


let show_expr_result =
  Result.show Token.pp Format.pp_print_string

let show_expr_results =
  Result.show (Fmt.Dump.list Token.pp) Format.pp_print_string

let test desc parser input expected show =
  let actual = P.parse parser input in
  if actual = expected then
    print ("%s %s" % (C.bright_green "✓", C.bright_white desc))
  else begin
    print ("%s %s" % (C.bright_red "✗", C.bright_white desc));
    print ("  - Expected: %s" % C.green (show expected));
    print ("  - Actual:   %s" % C.red (show actual))
  end


let (=>)  f x = f x show_expr_result
let (=>*) f x = f x show_expr_results



let x = Char 'x'
let y = Char 'y'

let () =
  (* Empty *)
  test "empty parser with empty input"
    P.empty Iter.empty => Error "parsing error: empty";

  test "empty parser with some input"
    P.empty (Iter.of_list [Char 'x']) => Error "parser did not consume entire input";

  test "parse the 'x' token"
    (P.exactly x) (Iter.of_list [x]) => Ok x;

  test "parse the 'x' token with remining input"
    (P.exactly x) (Iter.of_list [x; y]) => Error "parser did not consume entire input";

  (* Many *)
  test "parse many 'x' tokens with empty input"
    (P.many (P.exactly x)) (Iter.of_list []) =>* Ok [];

  test "parse many 'x' tokens with 'y' token as input"
    (P.many (P.exactly x)) (Iter.of_list [y]) =>* Error "parser did not consume entire input";

  test "parse many 'x' tokens"
    (P.many (P.exactly x)) (Iter.of_list [x; x; x; x; x; x; x]) =>* Ok [x; x; x; x; x; x; x];

  (* Some *)
  test "parse some 'x' tokens with empty input"
    (P.some (P.exactly x)) (Iter.of_list []) =>* Error "parsing error: empty";

  test "parse some 'x' tokens with 'y' token as input"
    (P.some (P.exactly x)) (Iter.of_list [y]) =>* Error "parsing error: token 'y' did not satisfy test";

  test "parse some 'x' tokens"
    (P.some (P.exactly x)) (Iter.of_list [x; x; x; x; x; x; x]) =>* Ok [x; x; x; x; x; x; x];


