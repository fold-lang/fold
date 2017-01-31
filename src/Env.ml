let () = Pervasives.print_endline "Env"

open Pure
open Lex
open Syntax

module Scope = Map.Make(String)


type t = {
  data : Expr.t Scope.t;
  next : t option;
}


let empty = {
  data = Scope.empty;
  next = None;
}


let define token expr env =
  let name = Token.to_string token in
  { env with data = Scope.add name expr env.data }


let rec lookup token env =
  let name = Token.to_string token in
  if Scope.mem name env.data then
    Some (Scope.find name env.data)
  else
    match env.next with
    | Some next -> lookup token next
    | None      -> None


let _lookup_infix name env =
  match lookup name env with
  | None -> None
  | Some expr ->
    begin match expr with
      | Form [Atom (_, Symbol "add_meta"); x] ->
        fail "todo"
      | _ ->
        fail "todo"
    end


