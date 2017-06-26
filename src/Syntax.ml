
open Pure
open Base

open Lex

module ID = struct
  type capitalized = string [@@deriving show]
  type lowercase   = string [@@deriving show]
end

module Pattern = struct
  type t = [
    | `Apply of t * t list
    | token
  ] [@@deriving show]

  let token x = x

  let apply f xs = `Apply (f, xs)
end

module Expression = struct
  type t = [
    | `Let of Pattern.t * t * t
    | `Apply of t * t list
    | `Lambda of Pattern.t list * t
    | token
  ] [@@deriving show]

  let let' pat expr body = `Let (pat, expr, body)
  let apply f xs = `Apply (f, xs)
  let lambda args body = `Lambda (args, body)
end

module Type = struct
  type t = [
    | `Constructor of ID.capitalized
    | `Var of ID.lowercase
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
    | `Type of ID.capitalized * ID.lowercase list * Type.t
  ] [@@deriving show]

  let val' pat expr : t = `Val (pat, expr)
  let def params expr = `Def (params, expr)
  let type' name params t = `Type (name, params, t)
end

