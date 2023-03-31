module G = Shaper_parser.Grammar
module L = Shaper_parser.Lexer
module P = Shaper_parser.Parser
module C = Fold_ast.Cons
open Prelude

type fl = Shaper.syntax

let ( let* ) = P.( let* )
let juxt_precedence = 300
let item_precedence = 11

let get_precedence str =
  match str.[0] with
  | '=' -> 1
  | '<' | '>' -> 2
  | '#' | '&' -> 2
  | '|' -> 2
  | '+' | '-' -> 3
  | '*' | '/' -> 4
  | _ -> 0

let check_is_operator_char x =
  match x with
  | '*' | '/' | '+' | '-' | '|' | '#' | '&' | '<' | '>' | '=' -> true
  | _ -> false

let eval_xml_to_fold : C.t Shaper_xml.eval =
  { elem =
      (fun ~tag ~attrs:_ children ->
        let f = C.lower "elem" in
        let args = [ C.string tag; C.list children ] in
        C.apply f args
      )
  ; data = (fun data -> C.apply (C.lower "text") [ C.string data ])
  }

let xml_grammar = Shaper_xml.make_grammar ~eval:eval_xml_to_fold

let form (left : fl) _g l =
  L.drop (L.Sym "!") l;
  match left with
  | Ident (Lower "calc") ->
    let x = P.parse_prefix Shaper_calc.grammar l in
    Result.map C.int x
  | Ident (Lower "xml") -> P.parse_prefix xml_grammar l
  | Ident (Lower kwd) -> Fmt.failwith "no such macro: %s" kwd
  (* TODO: classify left in error *)
  | _ -> failwith "invalid macro call form, must be kwd!"

let if_then_else =
  let precedence = Some juxt_precedence in
  let if_tok = L.Lower "if" in
  let else_tok = L.Lower "else" in
  let rule g l =
    let* () = P.consume if_tok l in
    let* cond = P.parse_prefix g l in
    let* if_true = P.parse ?precedence g l in
    if L.equal_token (L.pick l) else_tok then
      let* () = P.consume else_tok l in
      let* if_false = P.parse ?precedence g l in
      Ok (C.if_then_else cond if_true if_false)
    else Ok (C.if_then cond if_true)
  in
  rule

let splice =
  let tok = L.Sym "$" in
  let rule g l =
    L.drop tok l;
    let* x = P.parse_prefix g l in
    Ok (C.splice x)
  in
  rule

let prefix (tok : L.token) =
  match tok with
  (* `2 + 2` *)
  (* | Sym "`" -> Some prefix_quote *)
  | Sym "`" -> Some (P.prefix_unary ~precedence:juxt_precedence tok C.quote)
  | Sym "$" -> Some splice
  (* | Sym "|" -> Some (P.prefix_seq ~sep:(tok, 4) Fl.alt) *)
  | Lower "if" -> Some if_then_else
  | Lower "match" ->
    Some (P.prefix_unary ~precedence:juxt_precedence tok C.match_single)
  (* beats: `,` `=`; beaten by: `;` *)
  | Lower "let" -> Some (P.prefix_unary ~precedence:item_precedence tok C.let')
  | Lower "fn" ->
    Some (P.prefix_unary ~precedence:item_precedence tok C.fn_single)
  | Lower "module" ->
    Some (P.prefix_unary ~precedence:item_precedence tok C.module')
  | Lower "open" -> Some (P.prefix_unary ~precedence:item_precedence tok C.open')
  | Lparen ->
    P.prefix_scope Lparen Rparen (function
      | None -> Shaper.parens (Shaper.seq [])
      | Some items -> Shaper.parens items
      )
    |> Option.some
  | Lbrace ->
    P.prefix_scope Lbrace Rbrace (function
      | None -> Shaper.braces (Shaper.seq [])
      | Some items -> Shaper.braces items
      )
    |> Option.some
  | Lbracket ->
    P.prefix_scope Lbracket Rbracket (function
      | None -> Shaper.brackets (Shaper.seq [])
      | Some items -> Shaper.brackets items
      )
    |> Option.some
  | Int x -> Some (P.const (C.int x))
  | Lower x -> Some (P.const (C.lower x))
  | Upper x -> Some (P.const (C.upper x))
  | Sym x -> Some (P.const (Shaper.sym x))
  | String x -> Some (P.const (C.string x))
  | _ -> None

let infix (tok : L.token) =
  match tok with
  | Sym "`" -> None
  | Rparen | Rbrace | Rbracket -> Some P.infix_unbalanced
  | Lower "if"
  | Lower "when"
  | Lower "do"
  | Lower "then"
  | Lower "else"
  | Lower "match"
  | Lower "let"
  | Lower "fn"
  | Lower "module"
  | Lower "open"
  | Lower "val" -> Some P.infix_delimiter
  | Semi -> Some (P.infix_seq ~sep:(Semi, 10) (Shaper.seq ~sep:";"))
  | Sym "&" ->
    Some (P.infix_binary 19 tok (fun a b -> Shaper.shape "&" [ a; b ]))
  | Comma -> Some (P.infix_seq ~sep:(Comma, 20) (Shaper.seq ~sep:","))
  | Sym "=" -> Some (P.infix_binary 30 (L.Sym "=") C.binding)
  | Sym "|" -> Some (P.infix_seq ~sep:(tok, 40) C.alt)
  | Sym "->" -> Some (P.infix_binary 50 tok C.arrow)
  (* | Sym "->" -> Some (arrow, 50) *)
  | Sym "!" -> Some (form, 210)
  | Sym "." -> Some (P.infix_binary 220 (L.Sym ".") C.dot)
  | Sym s when check_is_operator_char s.[0] ->
    let precedence = 100 + get_precedence s in
    let rule =
      P.infix_binary precedence tok (fun a b -> C.apply (Shaper.sym s) [ a; b ])
    in
    Some rule
  | _ -> None

let g =
  G.make ~default_infix:(P.parse_infix_juxt Shaper.seq) ~prefix ~infix "fold"

let parse chan : C.t =
  let l = L.for_channel chan in
  P.run g l

let parse_string str : C.t =
  let l = L.for_string str in
  P.run g l
