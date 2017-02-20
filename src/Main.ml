
open Pure

open Fold
open Fold.Lex
open Fold.Syntax

module Env = Pratt.Env


(*

  "+" => x + y

  "+" => infix 30

  function x + y =
    Core.add x y


  rule add =
    expr "+" expr

  syntax a "+" b

  syntax "if" t "then" a "else" b =
    if t a b


  "if" => Form [Atom (<loc>, String "if");
                Atom (<loc>, Symbol "t");
                Atom (<loc>, String "then");
                Atom (<loc>, Symbol "a");
                Atom (<loc>, String "else");
                Atom (<loc>, Symbol "b")]

 *)


module Evaluator = struct
  let eval env expr =
    match expr with
    | Form (Atom (_, Symbol "syntax") :: rule) ->
      Env.define_syntax_rule rule
      env, expr

    | Form (Atom (_, Symbol "infix") :: _) ->
      fail "invalid infix declaration"

    | _ ->
      env, expr
end


let core_env =
  42


let main () =
  let rec loop env =
    let lexer = Lexer.from_channel stdin in
    let parse = Pratt.parse in
    (* print ~terminator:" " "->"; *)

    match parse lexer with
    | Ok expr ->
      let env', value = Evaluator.eval env expr in
      print (" = " ^ Expr.to_string value);
      loop env'

    | Error msg ->
      print (" * " ^ msg)
  in
    loop core_env


let () =
  main ()

