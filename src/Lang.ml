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
    Pratt.parse_prefix precedence >>= fun y ->
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
        parse_prefix precedence >>= fun y ->
        Parser.pure (Form [Atom token; x; y]) in
      (parse, precedence))


let define_syntax_operators grammar =
  grammar
  |> postfix 70 "*" (fun x -> Form [Atom (Symbol "*"); x])
  |> postfix 70 "+" (fun x -> Form [Atom (Symbol "+"); x])
  |> postfix 70 "?" (fun x -> Form [Atom (Symbol "?"); x])
  |> infix   30 "|" (fun x y -> Form [Atom (Symbol "|"); x; y])


let define_syntax =
  let parse =
    Parser.modify begin fun state ->
      State.{ state with grammar =
                define_syntax_operators (Grammar.new_scope state.grammar) }
    end >>= fun () ->
    Parser.advance >>= fun () ->
    expression >>= fun x ->
    Parser.modify begin fun state ->
      State.{ state with grammar = Grammar.pop_scope state.grammar }
    end >>= fun () -> Parser.pure x in
  Grammar.define_prefix (Symbol "syntax") parse



let grammar =
  let open Syntax.Expr in
  Grammar.init
    ~atom:(fun x -> singleton (Atom x))
    ~form:(fun x -> default_operator x or lazy (juxtaposition x))
    ()
  |> define_syntax
  |> between "(" ")" (fun x -> x)
  |> delimiter ")"

