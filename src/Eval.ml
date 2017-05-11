
open Pure
open Base

open Syntax
open Lex

module Pratt   = Pratt.Make(Expr)
module Grammar = Pratt.Grammar
open Pratt

let log = print ~flush:true ~file:stderr


let rec eval grammar expr =
  match expr with

  | Form (Atom (Symbol "syntax") :: []) ->
    fail "invalid infix declaration"

  | Form (Atom (Symbol "syntax") :: rule) ->
    (* log "Eval.eval: defining syntax rule..."; *)
    let name, peg = PEG.of_expr (Form rule) in
    let name = name or lazy (fail "anonymous syntax") in
    (* log ("Eval.eval: rule %s: %s\n" % (name, PEG.to_string peg)); *)
    (* XXX Assumed prefix for now *)
    let rule = PEG.to_pratt peg in
    let grammar' = Grammar.define_prefix (Symbol name)
        Parser.(rule >>= fun xs -> pure (Form (Atom (Symbol name) :: xs))) grammar
    in
      (* Grammar.dump grammar'; *)
      (* Fmt.pr "\n"; *)
      grammar', Expr.symbol "()"

  | _ ->
    grammar, expr


