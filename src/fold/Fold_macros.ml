type macro = Shaper.syntax list -> Shaper.syntax

module Env = Map.Make (String)

let env : macro Env.t ref = ref Env.empty
let defmacro tok p = env := Env.add tok p !env
let getmacro tok = Env.find_opt tok !env

(* --- unless --- *)

let unless_macro args =
  let module S = Shaper in
  match args with
  | [ cond; body ] ->
    S.shape "if" [ S.seq [ S.lower "not"; cond ]; body ]
    (* C.if_then (C.apply (C.lower "not") [ cond ]) body *)
  | _ -> failwith "rewrite: invalid unless args"

(* let () = defmacro "unless" unless_macro *)

(* foo *)

(*
    (define-syntax foo
      (lambda (stx)
        (syntax "I am foo")))
    ---
    syntax foo = fn _syn -> quote "I am foo";
  *)
let foo_macro _args = Shaper.string "I am foo"
let () = defmacro "foo" foo_macro
