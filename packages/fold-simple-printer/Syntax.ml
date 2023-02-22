type ident = Longident.t
type constant = Parsetree.constant

type keyword =
  | Prefix of string list
  | Infix of string
  | Mixfix of string list
  | Enclose of string * string

type t =
  | Void
  | Ident of ident
  | Constant of constant
  | Apply of t * t list
  | Form of string list * t list
  | Binding of (t * t)
  | Variant of t list
  | Seq of t list
  | Block of t list
  | Constraint of t * t
  | Field of t * t

(* form
   bind
   block
   seq
   apply *)

let ident ?(path = []) id =
  Ident (Option.get (Longident.unflatten (path @ [ id ])))
