
open Pure
open Fold
open Pratt
open Eval
open Lex
open Syntax

module Env = Fold.Pratt.Env


module Top = struct

  let let_syntax =
    let open Expr in
    form [symbol "syntax"; string "let"; symbol "a"; string "="; symbol "b"]


  let multiple = {|
    let x = 1
    let y = 2
  |}

  let test () =
    let (env0 : Env.t) = Env.default in
    let (env1 : Env.t), _ = eval env0 let_syntax in
    let input = Lexer.from_string multiple in
    let expected = Expr.(form [
        symbol ";;";
        form [symbol "let"; symbol "x"; int 1];
        form [symbol "let"; symbol "y"; int 2]])
    in

    print ("Syntax:    " ^ Expr.to_string let_syntax);
    print ("Input:     " ^ multiple);

    match Pratt.parse ~env:(env1 : Env.t) input with
    | Ok actual ->
      print_newline ();
      print ("Actual:   " ^ Expr.to_string actual);
      print ("Expected: " ^ Expr.to_string expected)

    | Error e -> print e
end


let () =
  Top.test ()



