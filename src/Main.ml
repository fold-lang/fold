
open Pure

open Fold
open Fold.Lex



let () =
  let rec loop grammar =
    print "Reading...";
    print ~terminator:"" "-> ";
    let lexer = Lexer.from_string (read_line ()) in
    print "Parsing...";
    match Pratt.run Lang.Parser.Statement.val' grammar lexer with
    | Ok syntax ->
      print (" = " ^ Lang.AST.Statement.show syntax);
      loop grammar

    | Error msg ->
      print (" * Error: " ^ Pratt.error_to_string msg);
      loop grammar
  in
  loop Pratt.Grammar.empty

