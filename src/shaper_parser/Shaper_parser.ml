module Lexer = Lexer
module Parser = Pratt_parser
module Grammar = Pratt_parser.Grammar

let ( let* ) = Parser.( let* )

let prefix (tok : Lexer.token) =
  let module P = Parser in
  let module S = Shaper in
  match tok with
  | Lparen ->
    P.prefix_scope Lparen Rparen (function
      | None -> S.parens (S.seq [])
      | Some items -> S.parens items
      )
    |> Option.some
  | Lbrace ->
    P.prefix_scope Lbrace Rbrace (function
      | None -> S.braces (S.seq [])
      | Some items -> S.braces items
      )
    |> Option.some
  | Lbracket ->
    P.prefix_scope Lbracket Rbracket (function
      | None -> S.brackets (S.seq [])
      | Some items -> S.brackets items
      )
    |> Option.some
  | Int x -> P.const (S.int x) |> Option.some
  | Lower x -> P.const (S.lower x) |> Option.some
  | Upper x -> P.const (S.upper x) |> Option.some
  | Sym x -> P.const (S.sym x) |> Option.some
  | String x -> P.const (S.string x) |> Option.some
  | _ -> None

let infix (tok : Lexer.token) =
  let module P = Parser in
  let module S = Shaper in
  match tok with
  | Rparen | Rbrace | Rbracket -> Some P.infix_unbalanced
  | Semi -> Some (P.infix_seq ~sep:(Semi, 1) (S.seq ~sep:";"))
  | Comma -> Some (P.infix_seq ~sep:(Comma, 5) (S.seq ~sep:","))
  | _ -> None

let grammar =
  Parser.Grammar.make
    ~default_infix:(Parser.parse_infix_juxt Shaper.seq)
    ~prefix ~infix "shaper"

let parse_string input =
  let l = Lexer.for_string input in
  Parser.run grammar l
