module S = Shaper.V03
module G = Shaper_parser.Grammar
module L = Shaper_parser.Lexer
module P = Shaper_parser.Parser
module Builder = Shaper_builder.V03

let ( let* ) = P.( let* )

let eval_xml_to_fold : S.syntax Shaper_xml.eval =
  { elem =
      (fun ~tag ~attrs:_ children ->
        let f = S.lower "elem" in
        let args = [ S.string tag; Builder.list children ] in
        Builder.apply f args
      )
  ; data = (fun data -> S.seq [ S.lower "text"; S.string data ])
  }

let xml_grammar = Shaper_xml.make_grammar ~eval:eval_xml_to_fold

let form left _g l =
  L.drop (L.Sym "!") l;
  match left with
  | S.Ident (S.Lower "calc") ->
    let x = P.parse_prefix Shaper_calc.grammar l in
    Result.map S.int x
  | S.Ident (S.Lower "xml") -> P.parse_prefix xml_grammar l
  | S.Ident (S.Lower kwd) -> Fmt.failwith "no such macro: %s" kwd
  (* TODO: classify left in error *)
  | _ -> failwith "invalid macro call form, must be kwd!"

let if_then_else =
  let precedence = Some 2 in
  let if_tok = L.Lower "if" in
  let then_tok = L.Lower "then" in
  let else_tok = L.Lower "else" in
  let rule g l =
    let* () = P.consume if_tok g l in
    let* cond = P.parse ?precedence g l in
    let* () = P.consume then_tok g l in
    let* then_expr = P.parse ?precedence g l in
    let* () = P.consume else_tok g l in
    let* else_expr = P.parse ?precedence g l in
    Ok (Builder.if_then_else cond then_expr else_expr)
  in
  P.Prefix (if_tok, rule)

let antiquote =
  let tok = L.Sym "$" in
  let rule g l =
    L.drop tok l;
    let* x = P.parse_prefix g l in
    Ok (Builder.antiquote x)
  in
  P.Prefix (tok, rule)

let alt_precendence = 30

(* [TODO] generalize *)
let prefix_alt =
  let alt_tok = L.Sym "|" in
  let rule g l =
    let* () = P.consume alt_tok g l in
    let* left = P.parse ~precedence:alt_precendence g l in
    let* items = P.parse_infix_seq ~sep:(alt_tok, alt_precendence) left g l in
    Ok (Builder.alt items)
  in
  P.Prefix (alt_tok, rule)

let rules =
  [ P.prefix (L.Sym "..") Builder.spread
  ; P.scope (L.Sym "`") (L.Sym "`") (function
      | None -> failwith "empty code quote"
      | Some code -> Builder.quote code
      )
  ; P.scope (L.Sym "<<") (L.Sym ">>") (function
      | None -> failwith "empty code quote"
      | Some code ->
        Fmt.epr ">>> %a@." S.dump code;
        Builder.quote code
      )
    (* ; P.prefix (L.Sym "$") Builder.antiquote *)
  ; antiquote
  ; if_then_else
  ; P.prefix ~precedence:2 (L.Lower "match") Builder.match_
  ; P.prefix ~precedence:2 (L.Lower "let") Builder.let_
  ; P.prefix ~precedence:2 (L.Lower "fn") Builder.fn
  ; P.prefix ~precedence:2 (L.Lower "module") Builder.module_
  ; P.prefix ~precedence:2 (L.Lower "open") Builder.open_
  ; P.prefix ~precedence:2 (L.Lower "val") Builder.val_
  ; P.invalid_infix_rule (L.Lower "then")
  ; P.invalid_infix_rule (L.Lower "else")
  ; P.invalid_infix_rule (L.Lower "let")
  ; P.invalid_infix_rule (L.Lower "fn")
  ; P.invalid_infix_rule (L.Lower "module")
  ; P.invalid_infix_rule (L.Lower "open")
  ; P.invalid_infix_rule (L.Lower "val")
  ; P.infix 10 (L.Sym "=") Builder.binding
  ; P.seq ~sep:(L.Sym "|", alt_precendence) Builder.alt
  ; prefix_alt
  ; P.infix 40 (L.Sym "->") Builder.arrow
  ; P.infix 80 (L.Sym ".") Builder.dot
  ; P.Infix (L.Sym "!", (form, 80))
  ]

let get_precedence str =
  match str.[0] with
  | '*' | '/' -> 40
  | '+' | '-' -> 30
  | '|' -> 21
  | '#' | '&' -> 20
  | '<' | '>' -> 20
  | '=' -> 10
  | _ -> 0

let check_is_operator_char x =
  match x with
  | '*' | '/' | '+' | '-' | '|' | '#' | '&' | '<' | '>' | '=' -> true
  | _ -> false

let default_infix x1 g l =
  let tok = L.pick l in
  match tok with
  | Sym "" -> invalid_arg "[BUG] Empty symbol string"
  | Sym s when check_is_operator_char s.[0] ->
    L.move l;
    let precedence = get_precedence s in
    let* x2 = P.parse ~precedence g l in
    Ok (Builder.apply (S.sym s) [ x1; x2 ])
  | _ -> P.infix_juxt S.seq x1 g l

let g =
  G.make ~default_prefix:Shaper_parser.const ~default_infix ~name:"fold" rules
  |> G.push_scope (G.scope_of_list Shaper_parser.rules)

let parse chan : S.syntax =
  let l = L.for_channel chan in
  P.run g l

let parse_string str : S.syntax =
  let l = L.for_string str in
  P.run g l
