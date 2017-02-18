
open Pure
open Fold
open Pratt
open Syntax


let test_parse_atomic_rules () =
  let cases = Expr.[
      "True"       , bool true;
      "False"      , bool false;
      "42"         , int 42;
      "3.14"       , float 3.14;
      "'x'"        , char 'x';
      "\"x\""      , string "x";
      "x"          , symbol "x";
    ] in
  List.iter (fun (str, rule) ->
      parse_string (parse_with_rule rule) str
      |> Result.print Expr.pp Format.pp_print_string;
      print "\n\n";
      assert (parse_string (parse_with_rule rule) str = Ok rule))
    cases


let () =
  test_parse_atomic_rules ()

