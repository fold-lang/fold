
open Pure

open Fold
open Fold.Lex
open Fold.Syntax


let () =
  let rec loop () =
    print ~terminator:"" "-> ";
    match Pratt.parse (Lexer.from_channel stdin) with
    | Ok expr ->
      print (" = " ^ Expr.to_string expr);
      loop ()

    | Error msg ->
      print (" * " ^ msg)
  in
    loop ()

