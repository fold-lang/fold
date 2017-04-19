
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
  lazy (Pratt.expression () >>= fun expr ->
        Parser.consume (Symbol e) >> lazy (Parser.pure expr))

let grammar =
  Grammar.empty
  |> Grammar.define_prefix "("       (group "(" ")")
  |> Grammar.define_prefix ")"        Pratt.invalid_prefix
  |> Grammar.define_infix  ")"       (Pratt.invalid_infix, 0)

  |> Grammar.define_prefix "__EOF__"  Pratt.invalid_prefix
  |> Grammar.define_infix  "__EOF__" (Pratt.invalid_infix, 0)


(* Tests *)

let (=>) = test grammar


let () = begin
  print ("-- %s" % C.bright_blue "Testing Fold.Pratt...");
  "x" => Ok x;
  "42" => Ok i42;
  "3.14" => Ok f3_14;
  "True" => Ok bT;

  "f x" => Ok (f1 x);
  "f x y z" => Ok (f3 x y z);
  "f True 42 3.14" => Ok (f3 bT i42 f3_14);

  "(x)" => Ok x;
  "f (f x)" => Ok (f1 (f1 x));
  "f 42 (f (f (True) 3.14) x (f x y))" => Ok (f2 i42 (f3 (f2 bT f3_14) x (f2 x y)));
  print ""
end


