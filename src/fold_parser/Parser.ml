module Parser = Pratt.Make (Lexer)

let ( let* ) = Parser.( >>= )

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

module Shaper = struct
  let term g : 'ast Parser.parser =
    let* current = Parser.current in
    let* () = Parser.advance in
    match current with
    | `Symbol x -> Parser.return (`Ident x)
    | `String x -> Parser.return (`String x)
    | `Int x -> Parser.return (`Integer x)
    | _ -> Parser.error (Parser.unexpected_token current)

  let group start stop grammar =
    let* () = Parser.consume (`Symbol start) in
    let* items = Parser.parse grammar in
    let* () = Parser.consume (`Symbol stop) in
    Parser.return (`Group ("{", "}", items))

  let sep sym grammar left =
    let* () = Parser.consume (`Symbol sym) in
    let* items =
      Parser.many
        (let* () = Parser.consume (`Symbol sym) in
         Parser.parse grammar
        )
    in
    Parser.return (`Seq (sym, left :: items))

  let parse l =
    let rules : 'a Parser.rule list =
      [ Parser.term term
      ; Parser.null (`Symbol "(") (group "(" ")")
      ; Parser.null (`Symbol "[") (group "[" "]")
      ; Parser.null (`Symbol "{") (group "{" "}")
      ; Parser.left 1 (`Symbol ",") (sep ",")
      ; Parser.left 1 (`Symbol ";") (sep ";")
      ]
    in
    Parser.run (Parser.parse (Parser.grammar rules)) l
end

type syntax =
  | Bool of bool
  | Int of int
  | String of string
  | Symbol of string
  | Var of string * syntax
  | If_then of syntax * syntax
  | Label of string * bool * syntax
  | Form of string * syntax list
  | If_then_else of syntax * syntax * syntax

let rec pp f t =
  match t with
  | Bool x -> Fmt.pf f "%b" x
  | Int x -> Fmt.pf f "%d" x
  | String x -> Fmt.pf f "%S" x
  | Symbol x -> Fmt.pf f "%s" x
  | Var (id, body) -> Fmt.pf f "var %s = (%a)" id pp body
  | Label (l, true, v) -> Fmt.pf f "~%s?:(%a)" l pp v
  | Label (l, false, v) -> Fmt.pf f "~%s?:(%a)" l pp v
  | Form (kwd, items) -> Fmt.pf f "(%s! %a)" kwd (Fmt.list ~sep:Fmt.sp pp) items
  | If_then (cond, if_true) -> Fmt.pf f "(if_then (%a) (%a))" pp cond pp if_true
  | If_then_else (cond, if_true, if_false) ->
    Fmt.pf f "(if_then (%a) (%a) (%a))" pp cond pp if_true pp if_false

let term g : 'ast Parser.parser =
  let* current = Parser.current in
  let* () = Parser.advance in
  match current with
  | `Symbol x -> Parser.return (Symbol x)
  | `String x -> Parser.return (String x)
  | `Int x -> Parser.return (Int x)
  | _ -> Parser.error (Parser.unexpected_token current)

let delimiter str = Parser.consume (`Symbol str)
let keyword str = Parser.consume (`Symbol str)

let quest _g left =
  let* () = keyword "?" in
  Parser.return (Form ("?", [ left ]))

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
    Parser.return (If_then_else (cond, if_true, if_false))
  | _ -> Parser.return (If_then (cond, if_true))

let label g : 'ast Parser.parser =
  let* () = keyword "~" in
  let* x = Parser.parse g in
  match x with
  | Symbol l -> (
    let* current = Parser.current in
    match current with
    | `Symbol ":" -> Parser.return (Label (l, false, Symbol l))
  )

let var grammar =
  let* () = keyword "var" in
  let* name = identifier in
  let* () = delimiter "=" in
  let* value = Parser.parse grammar in
  Parser.return (Var (name, value))

let rules : syntax Parser.rule list =
  [ Parser.term term
  ; Parser.null (`Symbol "var") var
  ; Parser.null (`Symbol "if") if_then_else
  ; Parser.null (`Symbol "~") label
  ; Parser.left 1 (`Symbol "?") quest
  ]

let parse = Parser.run (Parser.parse (Parser.grammar rules))
let error_to_string = Parser.error_to_string
