
open Pure

open Fold
open Fold.Lex



let () =
  let rec loop grammar =
    print ~terminator:"" "-> ";
    let lexer = Lexer.from_string (read_line ()) in
    match Pratt.run Lang.Parser.Statement.val' grammar lexer with
    | Ok syntax ->
      print (" = " ^ Lang.AST.Statement.show syntax);
      loop ()

    | Error msg ->
      print (" * Error: " ^ Pratt.error_to_string msg);
      loop ()
  in
  loop ()

