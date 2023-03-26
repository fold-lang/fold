module S = Shaper.V03
module G = Shaper_parser.Grammar
module L = Shaper_parser.Lexer
module P = Shaper_parser.Parser
module Builder = Shaper_builder.V03

let ( let* ) = P.( let* )

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
    let* () = P.consume if_tok l in
    let* cond = P.parse ?precedence g l in
    let* () = P.consume then_tok l in
    let* then_expr = P.parse ?precedence g l in
    let* () = P.consume else_tok l in
    let* else_expr = P.parse ?precedence g l in
    Ok (Builder.if_then_else cond then_expr else_expr)
  in
  rule

let antiquote =
  let tok = L.Sym "$" in
  let rule g l =
    L.drop tok l;
    let* x = P.parse_prefix g l in
    Ok (Builder.antiquote x)
  in
  rule

let alt_precendence = 30

(* [TODO] generalize *)
(* let prefix_alt = *)
(*   let alt_tok = L.Sym "|" in *)
(*   let rule g l = *)
(*     let* () = P.consume alt_tok l in *)
(*     let* left = P.parse ~precedence:alt_precendence g l in *)
(*     P.infix_seq ~sep:(alt_tok, alt_precendence) Builder.alt left g l *)
(*   in *)
(*   rule *)

let prefix_quote =
  let tok = L.Sym "`" in
  let rule (g : 'a P.grammar) l =
    let g' =
      { g with
        infix =
          (function
          | Sym "`" -> Some P.infix_delimiter
          | t -> g.infix t
          )
      }
    in
    let* () = P.consume tok l in
    Fmt.epr "Consumed first `@.";
    if L.pick l = tok then
      let* () = P.consume tok l in
      failwith "Empty code quote"
    else
      let* x = P.parse g' l in
      let* () = P.consume tok l in
      Ok (Builder.quote x)
  in
  rule

let prefix (tok : L.token) =
  match tok with
  | Sym ".." -> Some (P.prefix_unary tok Builder.spread)
  | Sym "<<" ->
    P.prefix_scope (Sym "<<") (Sym ">>") (function
      | None -> failwith "empty code block"
      | Some x -> Builder.quote x
      )
    |> Option.some
  | Sym "`" -> Some prefix_quote
  | Sym "$" -> Some antiquote
  | Lower "if" -> Some if_then_else
  | Lower "match" -> Some (P.prefix_unary ~precedence:2 tok Builder.match_)
  | Lower "let" -> Some (P.prefix_unary ~precedence:2 tok Builder.let_)
  | Lower "fn" -> Some (P.prefix_unary ~precedence:2 tok Builder.fn)
  | Lower "module" -> Some (P.prefix_unary ~precedence:2 tok Builder.module_)
  | Lower "open" -> Some (P.prefix_unary ~precedence:2 tok Builder.open_)
  | Lower "val" -> Some (P.prefix_unary ~precedence:2 tok Builder.val_)
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

let infix (tok : L.token) =
  match tok with
  | Sym "<<" -> None
  | Sym ">>" -> Some P.infix_delimiter
  | Sym "`" -> None
  | Rparen | Rbrace | Rbracket
  | Lower "if"
  | Lower "then"
  | Lower "else"
  | Lower "match"
  | Lower "let"
  | Lower "fn"
  | Lower "module"
  | Lower "open"
  | Lower "val" -> Some P.infix_delimiter
  | Sym "=" -> Some (P.infix_binary 10 (L.Sym "=") Builder.binding)
  | Sym "->" -> Some (P.infix_binary 40 (L.Sym "->") Builder.binding)
  | Sym "." -> Some (P.infix_binary 80 (L.Sym ".") Builder.binding)
  | Sym "!" -> Some (form, 80)
  | Sym "|" -> Some (P.infix_seq ~sep:(tok, alt_precendence) Builder.alt)
  | Sym s when check_is_operator_char s.[0] ->
    let precedence = get_precedence s in
    let rule =
      P.infix_binary precedence tok (fun a b -> Builder.apply (S.sym s) [ a; b ])
    in
    Some rule
  | Semi -> Some (P.infix_seq ~sep:(Semi, 1) (S.seq ~sep:";"))
  | Comma -> Some (P.infix_seq ~sep:(Semi, 5) (S.seq ~sep:","))
  | _ -> None

let g = G.make ~default_infix:(P.parse_infix_juxt S.seq) ~prefix ~infix "fold"

let parse chan : S.syntax =
  let l = L.for_channel chan in
  P.run g l

let parse_string str : S.syntax =
  let l = L.for_string str in
  P.run g l
