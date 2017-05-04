open Pure
open Base

open Lex
open Syntax

module Pratt   = Pratt.Make(Expr)
module Grammar = Pratt.Grammar

open Pratt

let juxtaposition token =
  let precedence = 90 in
  let parse x =
    Pratt.prefix precedence >>= fun y ->
    let list =
      match x with
      | Form xs -> List.append xs [y]
      | atom    -> [atom; y] in
    Parser.pure (Form list) in
  (parse, precedence)


let default_operator token =
  token
  |> Precedence.lookup
  |> Option.map (fun precedence ->
      let parse x =
        Parser.advance >>= fun () ->
        prefix precedence >>= fun y ->
        Parser.pure (Form [Atom token; x; y]) in
      (parse, precedence))


let syntax_scope = Rule.[
  Symbol "*", postfix 70   (fun x -> Form [Atom (Symbol "*"); x]);
  Symbol "+", postfix 70   (fun x -> Form [Atom (Symbol "+"); x]);
  Symbol "?", postfix 70   (fun x -> Form [Atom (Symbol "?"); x]);
  Symbol "|", infix   30   (fun x y -> Form [Atom (Symbol "|"); x; y]);
]


let syntax_rule =
  let parse =
    (* Add syntax_scope *)
    Parser.advance >>= fun () ->
    expression >>= fun x ->
    Parser.pure x in
  Grammar.Prefix parse



let grammar =
  let open Rule in
  let open Syntax.Expr in
  Grammar.init [
    Symbol "syntax", syntax_rule;
    Symbol "(",  group (Symbol ")");
    Symbol ")",  delimiter;
  ]
  ~atom:(fun x -> singleton (Atom x))
  ~form:(fun x -> default_operator x or lazy (juxtaposition x))

