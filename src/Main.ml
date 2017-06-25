
open Pure

open Fold
open Fold.Lex



let () =
  let rec loop () =
    print ~terminator:"" "-> ";
    let lexer = Lexer.from_string (read_line ()) in
    match Pratt.run Lang.Parser.Statement.parse lexer with
    | Ok syntax ->
      print (" = " ^ Lang.AST.Statement.show syntax);
      loop ()

    | Error msg ->
      print (" * Error: " ^ Pratt.error_to_string msg);
      loop ()
  in
  loop ()

