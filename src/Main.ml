
open Pure

open Fold
open Fold.Lex
open Fold.Syntax




let () =
  let rec loop grammar =
    print ~terminator:"" "-> ";
    let lexer = Lexer.from_string (read_line ()) in
    match Lang.Pratt.parse ~grammar lexer with
    | Ok [] -> print "<empty>"
    | Ok xs ->
      List.fold_left (fun g expr ->
          let g, value = Eval.eval grammar expr in
          print (" = " ^ Expr.to_string value);
          g) grammar xs
      |> loop

    | Error msg ->
      print (" * Error: " ^ msg);
      loop grammar
  in
  loop Lang.grammar

