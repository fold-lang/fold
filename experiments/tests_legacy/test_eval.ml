
open Pure

open Fold
open Fold.Syntax
open Fold.Eval

(* Translation combinators *)

let add_meta expr attr =
  Expr.(form [symbol "add_meta"; expr; attr])

let define name value =
  Expr.(form [symbol "define"; symbol name; value])


let test_values () =
  let open Expr in
  let assert_self_eval expr =
    assert (fst (eval expr Env.empty) = expr) in

  (* Atomic values evaluate to themselves. *)
  List.iter assert_self_eval
    [bool true; char 'x'; int 42; float 3.14; string "hello"]


let test_bindings () =
  let open Expr in
  (* Bindings update the environment. *)
  let result, env_with_x = eval (define "x" (int 1)) Env.empty in
  assert (result = symbol "()");
  assert (Env.(Scope.cardinal env_with_x.data) = 1);
  assert (Env.(env_with_x.next) = None);
  assert (Env.lookup (Lex.Location.empty, Lex.Symbol "x") env_with_x = Some (int 1));

  (* Symbols are resolved to bindings. *)
  assert (fst (eval (symbol "x") env_with_x) = int 1);

  (* TODO: Assert not defined *)
  assert
    (try ignore (eval (symbol "y") env_with_x); false
     with NameError "y" -> true)


let test_meta () =
  let open Expr in
  let assert_meta_eval (expr, expected_result) =
    assert (fst (eval expr Env.empty) = expected_result)
  in
  (* Meta annotations are ignored by the evaluation. *)
  List.iter assert_meta_eval
    [add_meta (int 42) (string "The answer."), int 42;
     add_meta (add_meta (int 42) (string "The answer.")) (string "Twice."), int 42]


let () =
  print "Testing Fold.eval...";
  test_values ();
  test_bindings ();
  test_meta ()

