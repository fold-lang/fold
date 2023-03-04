module Parser = Pratt.Make (Lexer)

module Syntax = struct
  type t =
    [ `Bool of bool
    | `Int of int
    | `String of string
    | `Symbol of string
    | `Var of string * t
    | `If_then of t * t
    | `If_then_else of t * t * t ]

  let rec pp f t =
    match t with
    | `Bool x -> Fmt.pf f "%b" x
    | `Int x -> Fmt.pf f "%d" x
    | `String x -> Fmt.pf f "%S" x
    | `Symbol x -> Fmt.pf f "%s" x
    | `Var (id, body) -> Fmt.pf f "var %s = (%a)" id pp body
    | `If_then (cond, if_true) ->
      Fmt.pf f "(if_then (%a) (%a))" pp cond pp if_true
    | `If_then_else (cond, if_true, if_false) ->
      Fmt.pf f "(if_then (%a) (%a) (%a))" pp cond pp if_true pp if_false
end

let ( let* ) = Parser.( >>= )
let delimiter str = Parser.consume (`Symbol str)
let keyword str = Parser.consume (`Symbol str)

let filter_map f =
  let* token = Parser.current in
  match f token with
  | Some x ->
    let* () = Parser.advance in
    Parser.return x
  | None -> Parser.error (Parser.unexpected_token token)

let identifier =
  filter_map (function
    | `Symbol x -> Some x
    | _ -> None
    )

let literal g : 'ast Parser.parser =
  let* current = Parser.current in
  let* () = Parser.advance in
  match current with
  | `Symbol x -> Parser.return (`Symbol x)
  | `String x -> Parser.return (`String x)
  | `Int x -> Parser.return (`Int x)
  | `Bool x -> Parser.return (`Bool x)
  | _ -> Parser.error (Parser.unexpected_token current)

let if_then_else grammar =
  let* () = keyword "if" in
  let* cond = Parser.parse grammar in
  let* () = delimiter "then" in
  let* if_true = Parser.parse grammar in
  let* current = Parser.current in
  match current with
  | `Symbol "else" ->
    let* () = Parser.advance in
    let* if_false = Parser.parse grammar in
    Parser.return (`If_then_else (cond, if_true, if_false))
  | _ -> Parser.return (`If_then (cond, if_true))

let var grammar =
  let* () = keyword "var" in
  let* name = identifier in
  let* () = delimiter "=" in
  let* value = Parser.parse grammar in
  Parser.return (`Var (name, value))

let rules : Syntax.t Parser.rule list =
  [ Parser.term literal
  ; Parser.null (`Symbol "var") var
  ; Parser.null (`Symbol "if") if_then_else
  ]

let parse = Parser.run (Parser.parse (Parser.grammar rules))
let error_to_string = Parser.error_to_string
