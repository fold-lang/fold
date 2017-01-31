
open Pure

open Fold
open Fold.Syntax
open Fold.Eval

(* Translation combinators *)

let add_meta expr attr =
  Expr.(form [symbol "add_meta"; expr; attr])

let define name value =
  Expr.(form [symbol "define"; symbol name; value])


(* def f x: ... *)

let test_macro_definition () =
  (* macro "if" t "then" a "else" b

     macro if $t then $a else $b *)

  let if_rule =
    Expr.(form [sym]) in
  assert true


let () =
  print "Testing macros...";
  test_macro_definition ()

