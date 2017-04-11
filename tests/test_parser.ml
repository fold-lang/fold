
open Pure
open Fold.Lex
open Fold.Syntax

module P = Fold.Parser


let test () =
  let x = Char 'x' in
  let y = Char 'y' in
  assert (P.parse P.empty Iter.empty = Error "parsing error: empty");
  assert (P.parse (P.exactly x) (Iter.of_list [x]) = Ok x);
  assert (P.parse (P.many (P.exactly x)) (Iter.of_list []) = Ok []);
  assert (P.parse (P.many (P.exactly x)) (Iter.of_list [y]) = Error "parser did not consume entire input");
  assert (P.parse (P.many (P.exactly x)) (Iter.of_list [x; x; x; x; x; x; x]) = Ok [x; x; x; x; x; x; x])


let () =
  test ()

