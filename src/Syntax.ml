
open Base
open Pure
open Lex

type expr =
  | Atom of Token.t
  | Form of expr list
  [@@deriving show]

module Expr = struct
  type t = expr

  include Printable.Make(struct
      type t = expr
      let pp = pp_expr
    end)

  include Monoid.Make(struct
      type nonrec t = t

      let empty =  Form []

      (* FIXME: Review *)
      let append self other =
        match (self, other) with
        | (Atom _ , Atom _)  -> Form [self; other]
        | (Form xs, Form ys) -> Form (xs ++ ys)
        | (Form xs, Atom _)  -> Form (xs ++ [other])
        | (Atom _ , Form ys) -> Form (self :: ys)
    end)

  let rec to_string = function
    | Atom x  -> Token.to_string x
    | Form xs -> "(%s)" % (String.concat " " (List.map to_string xs))

  let atom x  = Atom x
  let form xs = Form xs

  let bool   ?(loc = Location.empty) x  = Atom (Bool   x)
  let char   ?(loc = Location.empty) x  = Atom (Char   x)
  let float  ?(loc = Location.empty) x  = Atom (Float  x)
  let int    ?(loc = Location.empty) x  = Atom (Int    x)
  let string ?(loc = Location.empty) x  = Atom (String x)
  let symbol ?(loc = Location.empty) x  = Atom (Symbol x)
end

