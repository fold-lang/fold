type ident = Longident.t
type constant = Parsetree.constant

type keyword =
  | Prefix of string list (* <kwd1> <kwd2> ... *)
  | Infix of string (* ... <kwd> ... *)
  | Mixfix of string list (* <kwd> ... <kwd> ... *)
  | Enclose of string * string (* <kwd> ... <kwd> *)

type t =
  | Id of ident
  | Const of constant
  | Apply of t * t list
  | Form of keyword * t list
  | Variant of t list
  | List of t list
  | Block of t list
  | Field of t * t

let ident ?(path = []) id =
  Id (Option.get (Longident.unflatten (path @ [ id ])))
