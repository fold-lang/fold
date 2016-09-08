
open Pure
open Lex
open Syntax

module Scope = Map.Make(Literal)


type t = {
  data : Expr.t Scope.t;
  next : t option;
}


let empty = {
  data = Scope.empty;
  next = None;
}


let define name expr env =
  { env with data = Scope.add name expr env.data }


let rec lookup name env =
  if Scope.mem name env.data then
    Some (Scope.find name env.data)
  else
    match env.next with
    | Some next -> lookup name next
    | None      -> None

let lookup_infix name env =
  match lookup name env with
  | None -> None
  | Some expr ->
    begin match expr with
      | Form [Atom (_, Symbol "add_meta"); ...]

