module Pratt = Pratt
module Parser = Pratt.Make (Lexer)

module Syntax = struct
  type t =
    [ `Bool of bool
    | `Int of int
    | `String of string
    | `Symbol of string
    | `Var of string * t ]

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
    | _ -> None)

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

let rules =
  [ Parser.term literal
  ; (* XXX: WTF? *)
    (* TODO: Add context: "while parsing `var` expected x but got y". *)
    (* P.null (`Keyword "var") (fun g -> P.consume (`Keyword "varx") >>= fun () -> return (`Int 42)); *)
    Parser.null (`Symbol "var") var
  ; Parser.null (`Symbol "if") if_then_else
  ]

let parse = Parser.parse (Parser.grammar rules)

let input = {|
var x = if c then a else b
|}

let () =
  let lexer = Lexer.of_string input in
  match Parser.run parse lexer with
  | Ok syn -> Fmt.pr "%a@.@." Syntax.pp syn
  | Error Zero -> ()
  | Error e -> Fmt.pr "error: %s@." (Parser.error_to_string e)
