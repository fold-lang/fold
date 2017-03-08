
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




let main () =
  let rec loop env =
    let lexer = Lexer.from_channel stdin in

      print ">>>";
      Env.dump env;
      print "<<<";

    print ~terminator:" " "->";

    match Pratt.parse ~env lexer with
    | Ok expr ->
      let env', value = Eval.eval env expr in
      print (" = " ^ Expr.to_string value);
      loop env'

    | Error msg ->
      print (" * " ^ msg)
  in
    loop Env.default


let () =
  main ()

