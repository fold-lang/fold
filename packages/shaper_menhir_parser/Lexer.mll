{
(* open Lexing *)
open Parser

exception LexerError of string

let next_line lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let variable = (alpha)(alpha|digit|'_')* 
let keyword = variable '!'
let number = digit+('.' digit+)*

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule process_token = 
    parse
    | whitespace {process_token lexbuf}
    | number {NUMBER (float_of_string (Lexing.lexeme lexbuf))}
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "{" { LBRACE }
    | "}" { RBRACE }
    | "[" { LBRACKET }
    | "]" { RBRACKET }
    | "," {COMMA}
    | "->" { ARROW }
    | ":" { COLON }
    | "|" { PIPE }
    | "~" { TILDE }
    | "?" { QUEST }
    | "=" { EQ }
    | "." { DOT }
    | ".." { DOT_DOT }
    | ";" { SEMI }
    | newline { next_line lexbuf; process_token lexbuf }
    | variable as v {ID v}
    | keyword as kwd {KWD kwd}
    | eof { EOF }
    | _ {raise (LexerError ("Invalid Character: " ^ Lexing.lexeme lexbuf)) }