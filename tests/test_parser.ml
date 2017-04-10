
open Pure
open Fold.Lex

module P = Fold.Parser
let parse = Fold.Parser.parse


let test () =
  let x = Char 'x' in
  let y = Char 'y' in
  assert (parse P.empty Iter.empty = Error "parsing error: empty");
  assert (parse (P.exactly x) (Iter.of_list [x]) = Ok x);
  assert (parse (P.many (P.exactly x)) (Iter.of_list []) = Ok []);
  assert (parse (P.many (P.exactly x)) (Iter.of_list [y]) = Ok []);
  assert (parse (P.many (P.exactly x)) (Iter.of_list [x; x; x; x; x; x; x]) = Ok [x; x; x; x; x; x; x])


let () =
  test ()

