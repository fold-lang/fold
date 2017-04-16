

open Pure
open Fold
open Fold.Lex

module P = PEG.P
module C = Colors

module Result = struct
  include Result

  let map f = function
    | Ok x -> Ok (f x)
    | Error e -> Error e
end

let show = function
  | Ok xs -> "[" ^ String.concat "; " (List.map Token.to_string xs) ^ "]"
  | Error e -> P.error_to_string e


let (=>) (peg, input) (expected, leftover) =
  let parser = PEG.parse peg in
  let actual = Result.map fst (P.run parser input) in
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
let c = Symbol "c"

let iter = Iter.of_list

let () = begin
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
  PEG.(Terminal "a", iter []) => (Error (P.Unexpected_end { expected = a }), []);
  PEG.(Terminal "a", iter [b]) => (Error (P.Unexpected_token { expected = a; actual = b }), [b]);
  PEG.(Terminal "a", iter [a; b]) => (Ok [a], [b]);
  PEG.(Sequence [Terminal "a"; Terminal "b"], iter [a; b]) => (Ok [a; b], []);
  PEG.(Sequence [Terminal "a"; Terminal "b"], iter [a]) => (Error (P.Unexpected_end { expected = b }), []);
  PEG.(Sequence [Terminal "a"; Terminal "b"; Terminal "c"], iter [a; b; c]) => (Ok [a; b; c], []);
  PEG.(Alternative [Terminal "a"; Terminal "b"], iter [a]) => (Ok [a], []);
  PEG.(Alternative [Terminal "a"; Terminal "b"], iter [b]) => (Ok [b], []);
  PEG.(Alternative [Terminal "a"; Terminal "b"; Terminal "c"], iter [c]) => (Ok [c], []);
  PEG.(Optional (Terminal "a"), iter []) => (Ok [], []);
  PEG.(Optional (Terminal "a"), iter [a]) => (Ok [a], []);
  PEG.(Many (Terminal "a"), iter []) => (Ok [], []);
  PEG.(Many (Terminal "a"), iter [a; b]) => (Ok [a], [b]);
  PEG.(Many (Terminal "a"), iter [a; a; a]) => (Ok [a; a; a], []);
  PEG.(Some (Terminal "a"), iter []) => (Error (P.Unexpected_end { expected = a }), []);
  PEG.(Some (Terminal "a"), iter [b; a]) => (Error (P.Unexpected_token { expected = a; actual = b }), [b; a]);
  PEG.(Some (Terminal "a"), iter [a; b]) => (Ok [a], [b]);
  PEG.(Some (Terminal "a"), iter [a; a; a]) => (Ok [a; a; a], []);
end

