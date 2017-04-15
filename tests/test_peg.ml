

open Pure
open Fold
open Fold.Lex

module C = Colors

let show =
  Result.show (Fmt.Dump.list Token.pp) Format.pp_print_string


let (=>) (peg, input) (expected, leftover) =
  let parser = PEG.parse peg in
  let actual = Parser.parse parser input in
  let desc = Fmt.strf "(%s, %a)" (PEG.to_string peg) (Fmt.Dump.list Token.pp) (Iter.to_list input) in
  if actual = expected then
    print ("%s %s" % (C.bright_green "✓", C.bright_white desc))
  else begin
    print ("%s %s" % (C.bright_red "✗", C.bright_white desc));
    print ("  - Expected: %s" % C.green (show expected));
    print ("  - Actual:   %s" % C.red (show actual))
  end


let a = Symbol "a"
let b = Symbol "b"

let iter = Iter.of_list

let () =
  (* (p, s) => (r, s')
   *
   * where p  is the parsing expression
   *       s  is the initial input state of the parser
   *       r  is the produced result after appling [p]
   *       s' is the input state after applying [p]
   *)

  PEG.(Epsilon, iter []) => (Ok [], []);
  PEG.(Epsilon, iter [a]) => (Ok [], [a]);
  PEG.(Terminal "a", iter [a]) => (Ok [a], []);
  PEG.(Terminal "a", iter []) => (Error "unexpected end of file while reading `a`", []);
  PEG.(Terminal "a", iter [b]) => (Error "expected `a` but got `b`", [b]);
  PEG.(Terminal "a", iter [a; b]) => (Ok [a], [b]);
  PEG.(Many (Terminal "a"), iter []) => (Ok [], []);
  PEG.(Many (Terminal "a"), iter [a; b]) => (Ok [a], [b]);
  PEG.(Many (Terminal "a"), iter [a; a; a]) => (Ok [a; a; a], []);
  PEG.(Alternative [Terminal "a"; Terminal "b"], iter [a]) => (Ok [a], []);
  PEG.(Alternative [Terminal "a"; Terminal "b"], iter [b]) => (Ok [b], [])

