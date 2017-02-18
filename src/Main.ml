
open Pure

open Fold
open Fold.Lex
open Fold.Syntax


module Evaluator = struct
  let eval env expr =
    print "Evaluating stuff...";
    env, expr
end


let core_env =
  Env.empty

let main () =
  let rec loop env =
    let lexer = Lexer.from_channel stdin in
    let parse = Pratt.parse in
    print ~terminator:" " "->";

    match parse lexer with
    | Ok expr ->
      let env', value = Evaluator.eval env expr in
      print (" = " ^ Expr.to_string value);
      loop env'

    | Error msg ->
      print (" * " ^ msg);
      loop env
  in
    loop core_env


let () =
  main ()

