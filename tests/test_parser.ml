
open Pure
open Fold
open Lex

module P = Parser.Default

module C = Colors

module Result = struct
  include Result

  let map f = function
    | Ok x -> Ok (f x)
    | Error e -> Error e
end

let show_expr_result = function
  | Ok x -> Token.to_string x
  | Error e -> P.error_to_string e

let show_expr_results = function
  | Ok xs -> "[" ^ String.concat "; " (List.map Token.to_string xs) ^ "]"
  | Error e -> P.error_to_string e


let test desc parser input expected show =
  let actual = Result.map fst (P.run parser input) in
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
  print ("-- %s" % C.bright_blue "Testing Fold.Parser...");

  (* Empty *)
  test "empty parser with empty input"
    P.empty [] => Error P.Empty;

  test "empty parser with some input"
    P.empty [x] => Error P.Empty;

  test "parse the 'x' token"
    (P.exactly x) [x] => Ok x;

  test "parse the 'x' token with remining input"
    (P.exactly x) [x; y] => Ok x;

  (* Many *)
  test "parse many 'x' tokens with empty input"
    (P.many (P.exactly x)) [] =>* Ok [];

  test "parse many 'x' tokens with 'y' token as input"
    (P.many (P.exactly x)) [y] =>* Ok [];

  test "parse many 'x' tokens"
    (P.many (P.exactly x)) [x; x; x; x; x; x; x] =>* Ok [x; x; x; x; x; x; x];

  (* Some *)
  test "parse some 'x' tokens with empty input"
    (P.some (P.exactly x)) [] =>* Error (P.Unexpected_end { expected = x });

  test "parse some 'x' tokens with 'y' token as input"
    (P.some (P.exactly x)) [y] =>* Error (P.Unexpected_token { expected = x; actual = y });

  test "parse some 'x' tokens"
    (P.some (P.exactly x)) [x; x; x; x; x; x; x] =>* Ok [x; x; x; x; x; x; x];

  print ""


