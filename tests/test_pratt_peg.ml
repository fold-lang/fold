
module C = Colors

open Pure
open Fold
open Fold.Lex
open Fold.Syntax

module Pratt   = Pratt.Make(Expr)
module Grammar = Pratt.Grammar


open Pratt


let show = function
  | Ok xs -> "[" ^ String.concat "; " (List.map Expr.to_string xs) ^ "]"
  | Error e -> e

(* Testing function *)

let test grammar peg input expected =
  (* print ("peg = %s" % PEG.to_string peg); *)
  let parser = PEG.to_pratt peg in
  let actual = Pratt.run parser ~grammar (Lexer.from_string input) in
  if actual = expected then
    print ("%s %s" % (C.bright_green "✓", C.bright_white input))
  else begin
    print ("%s %s" % (C.bright_red "✗", C.bright_white input));
    print ("  - Expected: %s" % C.green (show expected));
    print ("  - Actual:   %s" % C.red (show actual))
  end


open Syntax.Expr

let juxtaposition tok =
  let precedence = 90 in
  let parse x =
    Pratt.prefix precedence >>= fun y ->
    let list =
      match x with
      | Form xs -> List.append xs [y]
      | atom    -> [atom; y] in
    Parser.pure (Form list) in
  (parse, precedence)


let grammar =
  let open Rule in
  Grammar.init [
    Symbol "==", infix   10 (fun x y -> form [symbol "=="; x; y]);
    Symbol "!=", infix   10 (fun x y -> form [symbol "!="; x; y]);
    Symbol "+",  prefix     (fun x   -> form [symbol "+"; x]);
    Symbol "+",  infix   30 (fun x y -> form [symbol "+"; x; y]);
    Symbol "-",  prefix     (fun x   -> form [symbol "+"; x]);
    Symbol "-",  infix   30 (fun x y -> form [symbol "-"; x; y]);
    Symbol "*",  infix   40 (fun x y -> form [symbol "*"; x; y]);
    Symbol "/",  infix   40 (fun x y -> form [symbol "/"; x; y]);
    Symbol "(",  group (Symbol ")");
    Symbol ")",  delimiter;
  ]
  ~atom:(fun x -> singleton (Atom x))
  ~form:juxtaposition


let () =
  let open PEG.DSL in

  let (=>) = test grammar (seq [term "let"; expr "a"; term "="; expr "b"]) in
  "let x = 0"     => Ok [symbol "x"; int 0];
  "let x = 2 + 2" => Ok [symbol "x"; form [symbol "+"; int 2; int 2]];


  let (=>) = test grammar (seq [term "if"; expr "t"; term "then"; expr "a"; term "else"; expr "b"])  in
  {|
    if True then
      'x'
    else
      'y'
  |} => Ok [bool true; char 'x'; char 'y'];

  {|
    if x - 1 == 0 then
      print "yes!"
    else
      print "no!"
  |} => Ok [form [symbol "=="; form [symbol "-"; symbol "x"; int 1]; int 0];
            form [symbol "print"; string "yes!"]; form [symbol "print"; string "no!"]];


  let (=>) = test grammar (seq [term "while"; expr "t"; term "do"; expr "a"; term "end"])  in
  {|
    while x / 2 != 0 do
      print "Hello, world!"
    end
  |} => Ok [form [symbol "!="; form [symbol "/"; symbol "x"; int 2]; int 0];
            form [symbol "print"; string "Hello, world!"]]






