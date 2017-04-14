
open Pure
open Fold.Lex

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

let iter = Iter.of_list

let () =
  (* Empty *)
  test "empty parser with empty input"
    P.empty (iter []) => Error "parsing error: empty";

  test "empty parser with some input"
    P.empty (iter [x]) => Error "parsing error: empty";

  test "parse the 'x' token"
    (P.exactly x) (iter [x]) => Ok x;

  test "parse the 'x' token with remining input"
    (P.exactly x) (iter [x; y]) => Error "parser did not consume entire input";

  (* Many *)
  test "parse many 'x' tokens with empty input"
    (P.many (P.exactly x)) (iter []) =>* Ok [];

  test "parse many 'x' tokens with 'y' token as input"
    (P.many (P.exactly x)) (iter [y]) =>* Error "parser did not consume entire input";

  test "parse many 'x' tokens"
    (P.many (P.exactly x)) (iter [x; x; x; x; x; x; x]) =>* Ok [x; x; x; x; x; x; x];

  (* Some *)
  test "parse some 'x' tokens with empty input"
    (P.some (P.exactly x)) (iter []) =>* Error "parsing error: empty";

  test "parse some 'x' tokens with 'y' token as input"
    (P.some (P.exactly x)) (iter [y]) =>* Error "parsing error: token 'y' did not satisfy test";

  test "parse some 'x' tokens"
    (P.some (P.exactly x)) (iter [x; x; x; x; x; x; x]) =>* Ok [x; x; x; x; x; x; x];


