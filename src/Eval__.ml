
open Pure
open Base

open Lex

module Pratt = Pratt.Make(Syntax)
module Grammar = Pratt.Grammar
open Pratt

let log = print ~flush:true ~file:stderr


let rec eval grammar (expr : Syntax.t) : (Grammar.t * Syntax.t) =
  match expr with

  | `Form ((`Symbol "syntax") :: []) ->
    fail "invalid infix declaration"

  | `Form ((`Symbol "syntax") :: rule) ->
    (* log "Eval.eval: defining syntax rule..."; *)
    let name, peg = PEG.of_expr (`Form rule) in
    let name = name or lazy (fail "anonymous syntax") in
    (* log ("Eval.eval: rule %s: %s\n" % (name, PEG.to_string peg)); *)
    (* XXX Assumed prefix for now *)
    let rule = PEG.to_pratt peg in
    let grammar' = Grammar.define_prefix (`Symbol name)
        Parser.(rule >>= fun xs -> pure (`Form ((`Symbol name) :: xs))) grammar
    in
      (* Grammar.dump grammar'; *)
      (* Fmt.pr "\n"; *)
      grammar', `Symbol "()"

  | _ ->
    grammar, expr


