
open Pure

open Fold
open Fold.Lex



let () =
  let rec loop state c =
    match Parser.Statement.parse state with
    | Ok (syntax, state') ->
      print ("-- %d --" % c);
      syntax |> Syntax.Statement.show |> print;
      loop state' (c + 1)

    | Error Pratt.Empty -> ()
    | Error e ->
      print (" * Error: " ^ Pratt.error_to_string e)
  in
  let lexer = Lexer.from_channel stdin in
  let token = Lexer.read lexer in
  loop Pratt.{ lexer; token } 0

