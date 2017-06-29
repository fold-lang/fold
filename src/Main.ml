
open Pure

open Fold
open Fold.Lex



let () =
  let rec loop () =
    print ~terminator:"" "-> ";
    let lexer = Lexer.from_channel stdin in
    match Pratt.run (Pratt.many Parser.Statement.parse) lexer with
    | Ok syntax ->
      List.iter (print << Syntax.Statement.show) syntax;
      loop ()

    | Error msg ->
      print (" * Error: " ^ Pratt.error_to_string msg);
      loop ()
  in
  loop ()

