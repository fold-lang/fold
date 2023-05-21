{
    type token =
      | Int of int
      | Lower of string
      | Upper of string
      | Sym of string
      | String of string
      | Lbrace
      | Rbrace
      | Lparen
      | Rparen
      | Lbracket
      | Rbracket
      | Comma
      | Semi
      | Eof

  let pp_token ppf token =
    match token with
    | Upper x -> Fmt.pf ppf "(Upper %s)" x
    | Lower x -> Fmt.pf ppf "(Lower %s)" x
    | Sym x -> Fmt.pf ppf "(Sym %s)" x
    | String x -> Fmt.pf ppf "(String %s)" x
    | Int x -> Fmt.pf ppf "(Int %d)" x
    | Lbrace -> Fmt.pf ppf "{"
    | Rbrace -> Fmt.pf ppf "}"
    | Lparen -> Fmt.pf ppf "("
    | Rparen -> Fmt.pf ppf ")"
    | Lbracket -> Fmt.pf ppf "["
    | Rbracket -> Fmt.pf ppf "]"
    | Comma -> Fmt.pf ppf ","
    | Semi -> Fmt.pf ppf ";"
    | Eof -> Fmt.pf ppf "EOF"

  let equal_token t1 t2 =
    Stdlib.(=) t1 t2

  let compare_token = Stdlib.compare

  type t = {
    lexbuf : Lexing.lexbuf;
    strbuf : Buffer.t;
    mutable token : token;
    mutable line_start : int;
    mutable line_count : int;
  }

  let update_loc_ lexer =
    lexer.line_count <- lexer.line_count + 1;
    lexer.line_start <- lexer.lexbuf.lex_abs_pos + lexer.lexbuf.lex_curr_pos

  let update_loc lexbuf file line absolute chars =
    let pos = lexbuf.Lexing.lex_curr_p in
    let new_file = match file with
                   | None -> pos.pos_fname
                   | Some s -> s
    in
    lexbuf.Lexing.lex_curr_p <- { pos with
      pos_fname = new_file;
      pos_lnum = if absolute then line else pos.pos_lnum + line;
      pos_bol = pos.pos_cnum - chars;
    }
}

(* Ident *)
let ident_lower_char = ['a'-'z' '_']
let ident_upper_char = ['A'-'Z']
let ident_inner_char = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']

let ident_upper = (ident_upper_char) ident_inner_char*
let ident_lower = (ident_lower_char) ident_inner_char*

let sym_char =
  ['!' '$' '%' '#' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let sym = sym_char+

let space = [' ' '\t' '\r']+
let digit = ['0'-'9']
let nonzero = ['1'-'9']
let int = (digit | nonzero digit+)

rule read lexer = parse
  | "//"[^'\n']* { read lexer lexbuf }
  | ident_lower { Lower (Lexing.lexeme lexbuf) }
  | ident_upper { Upper (Lexing.lexeme lexbuf) }
  | sym { Sym (Lexing.lexeme lexbuf) }
  | int { Int (int_of_string (Lexing.lexeme lexbuf)) }
  | '{' { Lbrace }
  | '}' { Rbrace }
  | '(' { Lparen }
  | ')' { Rparen }
  | '[' { Lbracket }
  | ']' { Rbracket }
  | ',' { Comma }
  | ';' { Semi }
  | '`' { Sym "`" }
  | '\'' { Sym "'" }
  | '"' {
    Buffer.clear lexer.strbuf;
    String (finish_string lexer lexbuf)
  }
  | "\n" {
    update_loc lexbuf None 1 false 0;
    read lexer lexbuf
  }
  | space { read lexer lexbuf }
  | eof { Eof }
  | _ { Fmt.failwith "Invalid token: %s" (Lexing.lexeme lexbuf) }

and finish_string lexer = parse
  | '"'  { Buffer.contents lexer.strbuf }
  | [^ '"' '\\']+ {
    let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
    Buffer.add_subbytes lexer.strbuf lexbuf.lex_buffer lexbuf.lex_start_pos len;
    finish_string lexer lexbuf
  }
    | eof { failwith "Unexpected end of input when reading a string" }

{
  let for_lexbuf lexbuf =
    let lexer = {
      lexbuf;
      strbuf = Buffer.create 64;
      token = Eof;
      line_count = 1;
      line_start = 0;
    } in
    lexer.token <- read lexer lexbuf;
    lexer

  let for_string s =
    let lexbuf = Lexing.from_string s in
    for_lexbuf lexbuf

  let for_channel ?file_name ic =
    let lexbuf = Lexing.from_channel ic in
    (match file_name with Some f -> Lexing.set_filename lexbuf f | _ -> ());
    for_lexbuf lexbuf

  let next lexer =
    lexer.token <- read lexer lexer.lexbuf;
    lexer.token

  let move lexer =
    lexer.token <- read lexer lexer.lexbuf

  let pick lexer =
    lexer.token

  let line_number lexer =
    lexer.line_count
  
  let drop expected lexer =
    let actual = pick lexer in
    begin if actual <> expected then
      Fmt.failwith "drop: invalid token: %a, expected %a" pp_token actual
        pp_token expected
    end;
    move lexer

  let is_eof token =
    match token with
    | Eof -> true
    | _ -> false

  let is_lower token =
    match token with
    | Lower _ -> true
    | _ -> false

  let is_upper token =
    match token with
    | Upper _ -> true
    | _ -> false

  let loc {lexbuf;_} = {
    Astlib.Location.loc_start = lexbuf.Lexing.lex_start_p;
    loc_end = lexbuf.lex_curr_p;
    loc_ghost = false
  }
}
