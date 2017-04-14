

open Pure
open Fold


let (=>) x y = ()

let a = Lex.Symbol "a"
let b = Lex.Symbol "b"


let () =
  (* (p, s) => (r, s')
   *
   * where p  is the parsing expression
   *       s  is the initial input state of the parser
   *       r  is the produced result after appling [p]
   *       s' is the input state after applying [p]
   *)

  PEG.(Epsilon, []) => (Ok [], []);
  PEG.(Epsilon, [a]) => (Ok [], [a]);
  PEG.(Terminal "a", [a]) => (Ok [a], []);
  PEG.(Terminal "a", []) => (Error (`Unexpected Lex.eof), []);
  PEG.(Terminal "a", [b]) => (Error (`Unexpected b), [b]);
  PEG.(Terminal "a", [a; b]) => (Ok [a], [b]);
  PEG.(Many (Terminal "a"), []) => (Ok [], []);
  PEG.(Many (Terminal "a"), [a; b]) => (Ok [a], [b]);
  PEG.(Many (Terminal "a"), [a; a; a]) => (Ok [a; a; a], []);
  PEG.(Alternative [Terminal "a"; Terminal "b"], [a]) => ([a], []);
  PEG.(Alternative [Terminal "a"; Terminal "b"], [b]) => ([b], [])

