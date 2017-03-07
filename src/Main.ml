
open Pure

open Fold
open Fold.Lex
open Fold.Syntax

module Env = Pratt.Env
module Parselet = Pratt.Parselet


let log = print ~file:stderr

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
  let rec eval env expr =
    match expr with

    (* Top-level Sequences *)
    | Form (Atom (_, Symbol ";;") :: []) ->
      fail "invalid sequenec sytnax"

    | Form (Atom (_, Symbol ";;") :: xs) ->
      log "Eval.eval: evaluating sequence...";

      List.fold_left
        (fun (env, _) expr -> eval env expr)
        (env, Expr.symbol "()")
        xs

    (* Syntax Rules *)
    | Form (Atom (_, Symbol "syntax") :: []) ->
      fail "invalid infix declaration"

    | Form (Atom (_, Symbol "syntax") :: rule) ->
      log "Eval.eval: defining syntax rule...";
      let env' =
        List.fold_left
          begin fun env' (name, parselet) ->
            Env.define_syntax name parselet env'
          end
          env
          (Parselet.create rule)
      in
        env', expr

    | _ ->
      env, expr
end


let main () =
  let rec loop env =
    let lexer = Lexer.from_channel stdin in

      print ">>>";
      Env.dump env;
      print "<<<";

    print ~terminator:" " "->";

    match Pratt.parse env lexer with
    | Ok expr ->
      let env', value = Evaluator.eval env expr in
      print (" = " ^ Expr.to_string value);
      loop env'

    | Error msg ->
      print (" * " ^ msg)
  in
    loop Env.default


let () =
  main ()

