
open Pure

open Fold
open Fold.Lex
open Fold.Syntax

module Evaluator = struct
  let eval env expr = env, expr
end


let core_env =
  Env.empty

let main () =
  let lexer = Lexer.from_channel stdin in
  let parse = Pratt.parse lexer in

  let rec loop env =
    print_string "-> ";

    match parse env with
    | Ok expr ->
      let env', value = Evaluator.eval env expr in
      Expr.print value;
      loop env'

    | Error msg ->
      print (" * " ^ msg);
      loop env in

  loop core_env


let () =
  main ()

