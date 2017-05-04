
open Pure

open Syntax
open Lex
open Pratt


let log = print ~file:stderr


let rec eval grammar expr =
  match expr with

  | Form (Atom (Symbol "syntax") :: []) ->
    fail "invalid infix declaration"

  | Form (Atom (Symbol "syntax") :: rule) ->
    log "Eval.eval: defining syntax rule...";
    let peg = PEG.of_expr (Form rule) in
    let _parser = PEG.to_pratt peg
    in
      grammar, Expr.symbol "()"

  | _ ->
    grammar, expr


