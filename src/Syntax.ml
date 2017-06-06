
open Base
open Pure
open Lex

type t = [
  | `Form of t list
  | `Val of t * t
  | token
] [@@deriving show, eq]


include Printable.Make(struct
  type nonrec t = t
  let pp = pp
end)

let to_string = show

let form   ?(loc = Location.empty) xs = `Form   xs
let bool   ?(loc = Location.empty) x  = `Bool   x
let char   ?(loc = Location.empty) x  = `Char   x
let float  ?(loc = Location.empty) x  = `Float  x
let int    ?(loc = Location.empty) x  = `Int    x
let string ?(loc = Location.empty) x  = `String x
let symbol ?(loc = Location.empty) x  = `Symbol x

