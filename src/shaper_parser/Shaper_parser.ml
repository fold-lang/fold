module S = Shaper.V03
module Lexer = Lexer
module Parser = Pratt_parser
module Grammar = Pratt_parser.Grammar

let const _g l =
  match Lexer.pick l with
  | Lexer.Int x ->
    Lexer.move l;
    Ok (S.int x)
  | Lexer.Lower x ->
    Lexer.move l;
    Ok (S.lower x)
  | Lexer.Upper x ->
    Lexer.move l;
    Ok (S.upper x)
  | Lexer.Sym x ->
    Lexer.move l;
    Ok (S.sym x)
  | t -> Fmt.failwith "not an atom: %a" Lexer.pp_token t

let grammar =
  Grammar.make ~default_prefix:const ~default_infix:(Parser.juxt S.seq)
    ~name:"shaper"
    [ Parser.scope Lexer.Lbrace Lexer.Rbrace (function
        | Some x -> S.braces x
        | None -> S.braces (S.seq [])
        )
    ; Parser.invalid_prefix_token Lexer.Rbrace
    ; Parser.seq ~sep:(Lexer.Comma, 70) (S.seq ~sep:",")
    ; Parser.seq ~sep:(Lexer.Semi, 1) (S.seq ~sep:";")
    ]

let parse_string input =
  let lexer = Lexer.for_string input in
  Parser.run grammar lexer
