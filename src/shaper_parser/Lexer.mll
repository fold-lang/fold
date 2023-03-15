{
    type token =
      | Int of int
      | Lower of string
      | Upper of string
      | Sym of string
      | Lbrace
      | Rbrace
      | Lparen
      | Rparen
      | Comma
      | Semi
      | Eof

  let pp_token ppf token =
    match token with
    | Upper x -> Fmt.pf ppf "(Upper %s)" x
    | Lower x -> Fmt.pf ppf "(Lower %s)" x
    | Sym x -> Fmt.pf ppf "(Sym %s)" x
    | Int x -> Fmt.pf ppf "(Int %d)" x
    | Lbrace -> Fmt.pf ppf "{"
    | Rbrace -> Fmt.pf ppf "}"
    | Lparen -> Fmt.pf ppf "("
    | Rparen -> Fmt.pf ppf ")"
    | Comma -> Fmt.pf ppf ","
    | Semi -> Fmt.pf ppf ";"
    | Eof -> Fmt.pf ppf "EOF"

  let compare_token = Stdlib.compare

  module Syn = Shaper.V03

  type t = {
    lexbuf : Lexing.lexbuf;
    mutable token : token;
    mutable line_start : int;
    mutable line_count : int;
  }

  let incr_line lexer =
    lexer.line_count <- lexer.line_count + 1;
    lexer.line_start <- lexer.lexbuf.lex_abs_pos + lexer.lexbuf.lex_curr_pos
}

(* Ident *)
let ident_lower_char = ['a'-'z' '_']
let ident_upper_char = ['A'-'Z']
let ident_inner_char = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']

let ident_upper = (ident_upper_char) ident_inner_char*
let ident_lower = (ident_lower_char) ident_inner_char*

let sym_char =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let sym = sym_char+

let space = [' ' '\t' '\r']+
let digit = ['0'-'9']
let nonzero = ['1'-'9']
let int = (digit | nonzero digit+)

rule read lexer = parse
  | ident_lower { Lower (Lexing.lexeme lexbuf) }
  | ident_upper { Upper (Lexing.lexeme lexbuf) }
  | sym { Sym (Lexing.lexeme lexbuf) }
  | int { Int (int_of_string (Lexing.lexeme lexbuf)) }
  | '{' { Lbrace }
  | '}' { Rbrace }
  | '(' { Lparen }
  | ')' { Rparen }
  | ',' { Comma }
  | ';' { Semi }
  | "\n" { incr_line lexer; read lexer lexbuf }
  | space { read lexer lexbuf }
  | eof { Eof }
  | _ { Fmt.failwith "Invalid token: `%s`" (Lexing.lexeme lexbuf) }

{
  let for_lexbuf lexbuf =
    let lexer = {
      lexbuf;
      token = Eof;
      line_count = 1;
      line_start = 0;
    } in
    lexer.token <- read lexer lexbuf;
    lexer

  let for_string s =
    let lexbuf = Lexing.from_string s in
    for_lexbuf lexbuf

  let for_channel ic =
    let lexbuf = Lexing.from_channel ic in
    for_lexbuf lexbuf

  let next lexer =
    lexer.token <- read lexer lexer.lexbuf;
    lexer.token

  let move lexer =
    lexer.token <- read lexer lexer.lexbuf

  let pick lexer =
    lexer.token
  
  let drop expected lexer =
    let actual = pick lexer in
    begin if actual <> expected then
      Fmt.failwith "invalid token: %a, expected %a" pp_token actual
        pp_token expected
    end;
    move lexer

  let is_eof token =
    match token with
    | Eof -> true
    | _ -> false
}
