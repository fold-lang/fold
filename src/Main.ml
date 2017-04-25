
open Pure

open Fold
open Fold.Lex
open Fold.Syntax
open Fold.Pratt

module Grammar = Fold.Pratt.Grammar

let (>>=) = Parser.(>>=)
let (>>)  = Parser.(>>)


let group s e =
  Parser.consume (Symbol s) >>
  lazy (Pratt.expression >>= fun expr ->
        Parser.consume (Symbol e) >> lazy (Parser.pure expr))

let grammar =
  Grammar.empty
  |> Grammar.define_prefix "("       (group "(" ")")

  |> Lang.define_delimiter ")"
  |> Lang.define_delimiter "__eof__"


let () =
  let rec loop () =
    print ~terminator:"" "-> ";
    match Pratt.parse ~grammar (Lexer.from_string (read_line ())) with
    | Ok expr ->
      print (" = " ^ Expr.to_string expr);
      loop ()

    | Error msg ->
      print (" * " ^ msg)
  in
    loop ()

