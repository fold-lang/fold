
open Pure

open Fold
open Fold.Lex
open Fold.Syntax

module Parser   = Pratt.Parser
module Parselet = Pratt.Parselet


module PEG(P : module type of Parser) = struct
  type t =
    [ `empty          (* empty *)
    | `seq of t * t   (* a b   *)
    | `alt of t * t   (* a | b *)
    | `opt of t       (* a?    *)
    | `many0 of t     (* a*    *)
    | `many1 of t     (* a+    *)
    | `term of string (* "x"   *)
    ]

  (*let generate self =*)
    (*let go self' =*)
      (*match self' with*)
      (*| `empty -> P.empty*)
      (*| `seq (a, b) -> go a >> lazy (go b)*)
      (*| `alt (a, b) -> go a <|> go b*)
      (*| `opt a -> go a <|> P.empty*)
      (*| `many0 a ->*)
    (*in*)
      (*go self*)
end


let syntax rule =
  fun input -> Error "empty"


let test_empty () =
  let p1 = syntax `empty in
  assert (p1 ""      = Error "empty");
  assert (p1 "x"     = Error "empty");
  assert (p1 "f z y" = Error "empty");
  assert (p1 "x + y" = Error "empty")



let () =
  test_empty ()

