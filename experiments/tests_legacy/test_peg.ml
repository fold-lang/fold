

open Pure
open Fold
open Fold.Lex

module P = Parser.Default
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
  let parser = PEG.to_parser peg in
  let actual = Result.map fst (P.run parser input) in
  let desc = Fmt.strf "(%s, %a)" (PEG.to_string peg) (Fmt.Dump.list Token.pp) input in
  if actual = expected then
    print ("%s %s" % (C.bright_green "✓", C.bright_white desc))
  else begin
    print ("%s %s" % (C.bright_red "✗", C.bright_white desc));
    print ("  - Expected: %s" % C.green (show expected));
    print ("  - Actual:   %s" % C.red (show actual))
  end


let a = `Symbol "a"
let b = `Symbol "b"
let c = `Symbol "c"

let () = begin
  print ("-- %s" % C.bright_blue "Testing Fold.PEG...");
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
  PEG.(Terminal "a", []) => (Error (P.Unexpected_end { expected = a }), []);
  PEG.(Terminal "a", [b]) => (Error (P.Unexpected_token { expected = a; actual = b }), [b]);
  PEG.(Terminal "a", [a; b]) => (Ok [a], [b]);
  PEG.(Sequence [Terminal "a"; Terminal "b"], [a; b]) => (Ok [a; b], []);
  PEG.(Sequence [Terminal "a"; Terminal "b"], [a]) => (Error (P.Unexpected_end { expected = b }), []);
  PEG.(Sequence [Terminal "a"; Terminal "b"; Terminal "c"], [a; b; c]) => (Ok [a; b; c], []);
  PEG.(Alternative [Terminal "a"; Terminal "b"], [a]) => (Ok [a], []);
  PEG.(Alternative [Terminal "a"; Terminal "b"], [b]) => (Ok [b], []);
  PEG.(Alternative [Terminal "a"; Terminal "b"; Terminal "c"], [c]) => (Ok [c], []);
  PEG.(Optional (Terminal "a"), []) => (Ok [], []);
  PEG.(Optional (Terminal "a"), [a]) => (Ok [a], []);
  PEG.(Many (Terminal "a"), []) => (Ok [], []);
  PEG.(Many (Terminal "a"), [a; b]) => (Ok [a], [b]);
  PEG.(Many (Terminal "a"), [a; a; a]) => (Ok [a; a; a], []);
  PEG.(Some (Terminal "a"), []) => (Error (P.Unexpected_end { expected = a }), []);
  PEG.(Some (Terminal "a"), [b; a]) => (Error (P.Unexpected_token { expected = a; actual = b }), [b; a]);
  PEG.(Some (Terminal "a"), [a; b]) => (Ok [a], [b]);
  PEG.(Some (Terminal "a"), [a; a; a]) => (Ok [a; a; a], []);
  print ""
end


(* let () = begin *)
(*   let open PEG in *)
(*   let open PEG.DSL in *)

(*   let (=>) = Test.(test (list (module Denotation))) in *)

(*   Test.group "Find denotation" [ *)
(*     find_name e => []; *)
(*     find_name (seq [term "x"])       => [`Prefix "x"]; *)
(*     find_name (seq [e; term "x"])    => [`Prefix "x"]; *)
(*     find_name (seq [e; e; term "x"]) => [`Prefix "x"]; *)
(*     find_name (seq [term "x" || term "y"]) => [`Prefix "x"; `Prefix "y"]; *)
(*     find_name (seq [seq [expr "a"; term "x"] || term "y"]) => [`Infix "x"; `Prefix "y"]; *)
(*   ]; *)
(* end *)

