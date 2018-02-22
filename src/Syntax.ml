
open Local

open Lex

module Name = struct
    type t =
      [ `ID of string
      | `Apply of t * t
      | `Dot of t * string ]
    [@@deriving show]

    let id x = `ID x

    let dot self str = `Dot (self, str)

    let apply a b = `Apply (a, b)
end


module Pattern = struct
  type t = [
    | `Tuple of t list
    | `Constructor of Name.t * t option
    | token
  ] [@@deriving show]

  let token x = (x :> t)

  let constructor name args = `Constructor (name, args)

  let tuple items = `Tuple items
end

module Expression = struct
  type t = [
    | `Let of (Pattern.t * t) list * t
    | `Apply of t * t list
    | `Lambda of Pattern.t list * t
    | `Tuple of t list
    | token
  ] [@@deriving show]

  let token x = (x :> t)
  let let' bindings body = `Let (bindings, body)
  let apply f xs = `Apply (f, xs)
  let lambda args body = `Lambda (args, body)
  let tuple items = `Tuple items
end

module Type = struct
  type t = [
    | `Constructor of Name.t
    | `Var of string
    | `Tuple of t list
  ] [@@deriving show]

  let constructor name = `Constructor name
  let var name = `Var name
  let tuple types = `Tuple types
end

module Statement = struct
  type t = [
    | `Val of Pattern.t * Expression.t
    | `Def of Pattern.t * Expression.t
    | `Type of Name.t * Name.t list * Type.t
  ] [@@deriving show]

  let val' pat expr : t = `Val (pat, expr)
  let def params expr = `Def (params, expr)
  let type' name params t = `Type (name, params, t)
end

module Module = struct
  type t = Statement.t list
  [@@deriving show]
end

