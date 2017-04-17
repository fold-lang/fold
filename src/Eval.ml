
open Pure

open Syntax
open Lex
open Pratt


let log = print ~file:stderr


let rec eval (env : Env.t) expr =
  match expr with

  (* Top-level Sequences *)
  | Form (Atom (Symbol ";;") :: []) ->
    fail "invalid sequenec sytnax"

  | Form (Atom (Symbol ";;") :: xs) ->
    log "Eval.eval: evaluating sequence...";

    List.fold_left
      (fun (env, _) expr -> eval env expr)
      (env, Expr.symbol "()")
      xs

  (* Syntax Rules *)
  | Form (Atom (Symbol "syntax") :: []) ->
    fail "invalid infix declaration"

  (* | Form (Atom (Symbol "syntax") :: rule) -> *)
  (*   log "Eval.eval: defining syntax rule..."; *)
  (*   let env' = *)
  (*     List.fold_left *)
  (*       begin fun env' (name, parselet) -> *)
  (*         (* XXX: Env.define_syntax name parselet env' *) *)
  (*           env' *)
  (*       end *)
  (*       env *)
  (*       (Parselet.create rule) *)
  (*   in *)
  (*     env', Expr.symbol "()" *)

  | _ ->
    env, expr


