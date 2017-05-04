
open Base
open Pure
open Lex

type expr =
  | Atom of Token.t
  | Form of expr list
  [@@deriving show, eq]

module Expr = struct
  type t = expr
  [@@deriving show, eq]

  include Printable.Make(struct
      type t = expr
      let pp = pp_expr
    end)

  let dump = pp

  let rec pp ppf self =
    let open Fmt in
    match self with
    | Atom v -> pf ppf "%s" (Token.to_string v)
    | Form vs ->
      let rec loop = function
        | [] -> ()
        | v :: vs ->
            if vs = [] then (pf ppf "@[%a@]" pp v) else
            (pf ppf "@[%a@]@ " pp v; loop vs)
      in
        pf ppf "@[<1>("; loop vs; pf ppf ")@]"


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

