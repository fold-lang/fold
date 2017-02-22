
open Pure

open Fold
open Fold.Lex
open Fold.Syntax

module Env = Pratt.Env
module Parselet = Pratt.Parselet


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

    (* Syntax Rules *)
    | Form (Atom (_, Symbol "syntax") :: []) ->
      fail "invalid infix declaration"

    | Form (Atom (_, Symbol "syntax") :: rule) ->
      print "Eval.eval: defining syntax rule...";
      let name, parser = Parselet.create rule |> Result.force in
      let env' = Env.define_syntax name parser env in
      env', expr

    | _ ->
      env, expr
end


let core_env =
  Env.empty
  |> Env.define_syntax "<EOF>" (Parselet.Infix  ((fun x -> undefined ()), 0))


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
  loop core_env


let () =
  main ()

