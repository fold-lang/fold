
open Pure
open Fold
open Fold.Pratt
open Fold.Lex
open Fold.Syntax

module Grammar = Fold.Pratt.Grammar
module C = Colors


let (>>=) = Parser.(>>=)
let (>>)  = Parser.(>>)


let show = function
  | Ok x -> Expr.to_string x
  | Error e -> e


(* Testing function *)

let test grammar input expected =
  let lexer = Lexer.from_string input in
  let actual = Pratt.parse ~grammar lexer in
  if actual = expected then
    print ("%s %s" % (C.bright_green "✓", C.bright_white input))
  else begin
    print ("%s %s" % (C.bright_red "✗", C.bright_white input));
    print ("  - Expected: %s" % C.green (show expected));
    print ("  - Actual:   %s" % C.red (show actual))
  end


(* Some helper definitions *)

let x, y, z =
  let s = fun x -> Atom (Symbol x) in
  s "x", s "y", s "z"

let i42, i0, i1, f3_14, bT =
  let a = fun x -> Atom x in
  a (Int 42), a (Int 0), a (Int 1), a (Float 3.14), a (Bool true)

let f1, f2, f3 =
  (fun x     -> Form [Atom (Symbol "f"); x]),
  (fun x y   -> Form [Atom (Symbol "f"); x; y]),
  (fun x y z -> Form [Atom (Symbol "f"); x; y; z])


(* Definitions *)

let group s e =
  Parser.consume (Symbol s) >>
  lazy (Pratt.expression >>= fun expr ->
        Parser.consume (Symbol e) >> lazy (Parser.pure expr))

let grammar =
  Grammar.empty
  |> Grammar.define_prefix "("       (group "(" ")")

  |> Lang.define_delimiter ")"
  |> Lang.define_delimiter "__EOF__"


let plus_rule input =
  let open PEG.DSL in
  let peg = seq [t "let"; n "a"; t "="; n "b"] in
  let parser = PEG.to_pratt peg in
  let state = Pratt.State.init ~grammar ~lexer:(Lexer.from_string input) () in
  print ("peg = %s" % PEG.to_string peg);
  match Parser.run parser state with
  | Ok (r, _) -> Fmt.pr "Ok: [%s]\n" (String.concat ", " (List.map Expr.to_string r))
  | Error e -> Fmt.pr "Error: %s\n" (Parser.error_to_string e)


let () =
  plus_rule "let result = 42"







