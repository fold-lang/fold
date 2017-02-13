
open Pure
open Lex
open Syntax

module M = Map.Make(String)

type t = {
  precedence : Int.t  M.t;
  infix      : Expr.t M.t;
  prefix     : Expr.t M.t;
  next       : t option;
}


let empty = {
  precedence = M.empty;
  prefix     = M.empty;
  infix      = M.empty;
  next       = None;
}


(* Define *)

let define_precedence literal precedence env =
  let name = Literal.to_string literal in
  { env with precedence = env.precedence |> M.add name precedence }

let define_prefix literal expr env =
  let name = Literal.to_string literal in
  { env with prefix = env.prefix |> M.add name expr }

let define_infix literal expr env =
  let name = Literal.to_string literal in
  { env with infix = env.infix |> M.add name expr }


(* Lookup *)

let rec lookup_precedence literal env =
  let name = Literal.to_string literal in
  if M.mem name env.precedence then
    M.find name env.precedence
  else
    match env.next with
    | Some next -> lookup_precedence literal next
    | None      -> 0

let rec lookup_prefix literal env =
  let name = Literal.to_string literal in
  if M.mem name env.prefix then
    Some (M.find name env.prefix)
  else
    match env.next with
    | Some next -> lookup_prefix literal next
    | None      -> None

let rec lookup_infix literal env =
  let name = Literal.to_string literal in
  if M.mem name env.infix then
    Some (M.find name env.infix)
  else
    match env.next with
    | Some next -> lookup_infix literal next
    | None      -> None

