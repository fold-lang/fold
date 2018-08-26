
module P = Fold.Parser
module L = Fold.Lex.Lexer
module S = Fold.Syntax
module Syntax = Fold.Syntax

module Expr = Fold.Syntax.Expr
module Pat = Fold.Syntax.Pattern

open Nanotest

(* Local Testing DSL *)

let test parser msg input expected =
  let actual = P.run parser L.(to_stream (of_string input)) in
  let testable = result (module Expr) (testable (Fmt.of_to_string P.P.error_to_string)) in
  test ~verbose:true testable msg ~expected ~actual


(* Local Helper Definitions *)

let evar x = Syntax.Expr.name (Syntax.Name.lower x)
let pvar x = Fold.Syntax.Pattern.var x
let int x = Expr.constant (Fold.Syntax.Constant.int x)

let a, b, c, d, e =
  pvar "a", pvar "b", pvar "c", pvar "d", pvar "e"

let f args =
  `Application (`Symbol "f", args)

let (+) a b = `Application (`Symbol "+", [a; b])

let tuple items = `Tuple items


(* Tests *)

(* let value_path () = *)
(*   let test input expected = *)
(*     let actual = P.run P.value_path L.(to_stream (of_string input)) in *)
(*     let testable = result (module S.Name) (testable (Fmt.of_to_string P.P.error_to_string)) in *)
(*   Nanotest.test ~verbose:true testable input ~expected ~actual in *)

(*   group "Simple value path" [ *)

(*     (* test "variable" *) *)
(*     (*   "a" ==> Ok (S.(Expression.name (Name.id "a"))); *) *)

(*     test "M.a" ==> Ok (S.Name.make ~path:["M"] "a"); *)
(*   ] *)


(* let test = test P.Expression.parser *)

let parents () =
  group "Units, tuples and expression groups" [
    (* test "parse unit value" *)
    (*   "()" ==> Ok (Expr.token (`Symbol "()")); *)

    (* test "parse simple group" *)
    (*   "(a)" ==> Ok (Expr.token a); *)

    (* test "parse compound group" *)
    (*   "(f a + b)" ==> Ok (f [a] + b); *)

    (* test "parse simple pair" *)
    (*   "(a, b)" ==> Ok (tuple [a; b]); *)

    (* test "parse simple triple" *)
    (*   "(a, b, c)" ==> Ok (tuple [a; b; c]); *)

    (* test "parse simple quadruple" *)
    (*   "(a, b, c, d)" ==> Ok (tuple [a; b; c; d]); *)
  ]

let invalid () =
  group "Invalid cases" [
    (* test "reject unclosed unit value" *)
    (*   "(" ==> Error (P.P.unexpected_end ()); *)

    (* test "reject on unclosed tuple value 1" *)
    (*   "(1," ==> Error (P.P.unexpected_token (`Symbol ",") ~expected:(`Symbol ")")); *)

    (* test "reject on unclosed tuple value 2" *)
    (*   "(1,2" ==> Error (P.P.unexpected_end ~expected:(`Symbol ")") ()); *)
  ]

let bindings () =
  let test = test P.Expr.parser in
  group "Simple bindings" [
    test "parse simple let-binding"
      "let a = 1 in a" ==> Ok (Expr.let' [pvar "a", int 1] (evar "a"));

    (* test "parse two let-bindings" *)
    (*   "let a = 2, b = 2 in a + b" ==> Ok (Expr.let' [a, int 2; b, int 2] (a + b)); *)

    (* test "parse three let-bindings" *)
    (*   "let a = 1, b = 2, c = 3 in a + b + c" *)
    (*     ==> Ok (Expr.let' [a, int 1; b, int 2; c, int 3] (a + b + c)); *)

    (* test "parse two sequential let-bindings" *)
    (*   "let a = 2 in let b = 2 in a + b" *)
    (*     ==> Ok Expr.(let' [a, int 2] (let' [b, int 2] (a + b))); *)

    (* test "parse two nested let-bindings" *)
    (*   {| *)
    (*   let a = *)
    (*     let b = c in *)
    (*     b + d *)
    (*   in *)
    (*     a + d *)
    (*   |} ==> Ok Expr.(let' [a, let' [b, c] (b + d)] (a + d)); *)
  ]


let () =
  bindings ()

