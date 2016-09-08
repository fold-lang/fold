
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

  let rec to_string = function
    | Atom x  -> Token.to_string x
    | Form xs -> "(%s)" % (String.concat " " (List.map to_string xs))

  let atom x  = Atom x
  let form xs = Form xs

  let bool   ?(loc = Location.empty) x  = Atom (loc, Bool   x)
  let char   ?(loc = Location.empty) x  = Atom (loc, Char   x)
  let float  ?(loc = Location.empty) x  = Atom (loc, Float  x)
  let int    ?(loc = Location.empty) x  = Atom (loc, Int    x)
  let string ?(loc = Location.empty) x  = Atom (loc, String x)
  let symbol ?(loc = Location.empty) x  = Atom (loc, Symbol x)
  let name   ?(loc = Location.empty) x  = Atom (loc, Name   x)
end

