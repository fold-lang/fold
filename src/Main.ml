
open Pure

open Fold
open Fold.Lex


module P = Parser.Make(OCaml)

let () =
  let rec loop state c =
    match P.Statement.parse state with
    | Ok (syntax, state') ->
      print ("-- %d --" % c);
      Printast.top_phrase Fmt.stdout (Parsetree.Ptop_def [syntax]);
      loop state' (c + 1)

    | Error Pratt.Empty -> ()
    | Error e ->
      print (" * Error: " ^ Pratt.error_to_string e)
  in
  let lexer = Lexer.from_channel stdin in
  let token = Lexer.read lexer in
  loop Pratt.{ lexer; token } 0

