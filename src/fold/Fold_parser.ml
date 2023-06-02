module P = Pratt
module G = Pratt.Grammar
module L = Pratt.Lexer
module S = Shaper
module C = Fold_ast.Cons
module Prec = Fold_precedence
open Prelude

let ( let* ) = P.( let* )

module Shaper_parser = struct
  let prefix (tok : L.token) =
    match tok with
    | Lparen ->
      P.prefix_scope Lparen Rparen (function
        | None -> C.parens (Shaper.seq [])
        | Some x -> C.parens x
        )
      |> Option.some
    | Lbrace ->
      P.prefix_scope Lbrace Rbrace (function
        | None -> Shaper.braces (Shaper.seq [])
        | Some x -> Shaper.braces x
        )
      |> Option.some
    | Lbracket ->
      P.prefix_scope Lbracket Rbracket (function
        | None -> Shaper.brackets (Shaper.seq [])
        | Some x -> Shaper.brackets x
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
    | Rparen | Rbrace | Rbracket -> Some P.infix_unbalanced
    | Semi -> Some (P.infix_seq_opt ~sep:(tok, Prec.semi) Shaper.semi)
    | Comma -> Some (P.infix_seq ~sep:(tok, Prec.comma) Shaper.comma)
    | Eof -> Some P.eof
    | _ -> None

  let grammar =
    G.make
      ~default_infix:(P.parse_infix_juxt ~precedence:Prec.juxt Shaper.seq)
      ~prefix ~infix "shaper"

  let parse_string str : C.t =
    let l = L.for_string str in
    P.run grammar l
end

let check_is_operator_char x =
  match x with
  | '*' | '/' | '+' | '-' | '|' | '#' | '&' | '<' | '>' | '=' -> true
  | _ -> false

let macro_call (left : fl) _g l =
  let* () = P.consume (L.Sym "!") l in
  let loc = L.loc l in
  match left with
  | Ident (Lower kwd) ->
    let* arg = P.parse_prefix Shaper_parser.grammar l in
    let args =
      match arg with
      | Shaper.Scope ("(", Shaper.Seq items, ")") -> items
      | Shaper.Scope _ -> [ arg ]
      | _ -> failwith "macro arguments must be scoped"
    in
    let shape = Shaper.shape ~loc kwd args in
    Ok shape
  | _ -> failwith "invalid macro call form, must be kwd!"

let parse_infix_dot (left0 : fl) g l =
  let loc = L.loc l in
  let dot_tok = L.Sym "." in
  let* () = P.consume dot_tok l in
  let rec parse_ident path =
    let* (x : fl) = P.parse_prefix g l in
    let path = List.append path [ x ] in
    let tok = L.pick l in
    match x with
    | Ident (Upper _) when L.equal_token tok dot_tok ->
      let* () = P.consume dot_tok l in
      parse_ident path
    | Ident (Lower _) when L.equal_token tok dot_tok ->
      let* () = P.consume dot_tok l in
      let expr =
        match path with
        | [ a ] -> a
        | _ -> Shaper.shape ~loc "ident" path
      in
      parse_field [ expr ]
    | Ident _ -> Ok (Shaper.shape ~loc "ident" path)
    | _ -> failwith "invalid dot component"
  and parse_field path =
    let* (x : fl) = P.parse_prefix g l in
    match x with
    | Ident (Upper id) -> Fmt.failwith "invalid field: %s" id
    | Ident (Lower _) ->
      let path = List.append path [ x ] in
      let tok = L.pick l in
      if L.equal_token tok dot_tok then
        let* () = P.consume dot_tok l in
        parse_field path
      else Ok (Shaper.shape ~loc "field" path)
    | Scope ("(", ident_fl, ")") ->
      let expr =
        match path with
        | [ a ] -> a
        | _ -> Shaper.shape ~loc "field" path
      in
      Ok (Shaper.shape ~loc "field" [ expr; ident_fl ])
    | _ -> assert false
  in
  match left0 with
  | Ident (Upper _) -> parse_ident [ left0 ]
  | _ -> parse_field [ left0 ]

let parse_label g l =
  let* () = P.consume (L.Sym "~") l in
  match L.pick l with
  | Lower label -> (
    L.move l;
    match L.pick l with
    | Sym "=" ->
      L.move l;
      let* x = P.parse_prefix g l in
      Ok (C.label label x)
    | Sym "?=" ->
      L.move l;
      let* x = P.parse_prefix g l in
      Ok (C.label_opt label x)
    | Sym "?" ->
      L.move l;
      Ok (C.label_opt_pun label)
    | _ -> Ok (C.label_pun label)
  )
  | Lparen ->
    let* x = P.parse_prefix g l in
    let loc = L.loc l in
    Ok (Shaper.shape ~loc "~" [ x ])
  | tok -> Fmt.failwith "invalid label: %a" L.pp_token tok

let prefix (tok : L.token) =
  match tok with
  | Lower
      ( ("fn" | "if" | "match" | "quote" | "unquote" | "for" | "while" | "try")
      as kwd
      ) ->
    Some
      (P.prefix_unary ~precedence:Fold_precedence.juxt tok (function
        | S.Seq items -> C.shape kwd items
        | x -> C.shape kwd [ x ]
        )
        )
  | Lower
      ( ("let" | "rec" | "module" | "mod" | "sig" | "open" | "do" | "type") as
      kwd
      ) ->
    Some
      (P.prefix_unary ~precedence:Fold_precedence.item tok (function
        | S.Seq items -> C.shape kwd items
        | x -> C.shape kwd [ x ]
        )
        )
  (* prefix tight *)
  | Sym (("#" | "'") as kwd) ->
    let rule g l =
      let* () = P.consume tok l in
      let* x = P.parse_prefix g l in
      Ok (S.shape kwd [ x ])
    in
    Some rule
  (* prefix attr *)
  | Sym "@" ->
    let rule g l =
      let* () = P.consume tok l in
      let* attr = P.parse_prefix g l in
      let* payload = P.parse_prefix g l in
      Ok (S.shape "@" [ attr; payload ])
    in
    Some rule
  (* prefix label *)
  | Sym "~" -> Some parse_label
  | _ -> Shaper_parser.prefix tok

let infix (tok : L.token) =
  match tok with
  | Lower
      ( "when"
      | "do"
      | "end"
      | "then"
      | "else"
      | "match"
      | "let"
      | "rec"
      | "fn"
      | "module"
      | "mod"
      | "sig"
      | "type"
      | "open"
      | "val" ) -> Some P.infix_delimiter
  | Lower "if" ->
    Some (P.infix_binary 90 tok (fun a b -> Shaper.shape "_if_" [ a; b ]))
  | Lower (("to" | "downto") as kwd) ->
    Some (P.infix_binary 90 tok (fun a b -> Shaper.shape kwd [ a; b ]))
  | Sym "#" -> None
  | Sym "&" ->
    Some (P.infix_binary Prec.ampr tok (fun a b -> Shaper.shape "&" [ a; b ]))
  | Sym "=" -> Some (P.infix_right_binary Prec.equal (L.Sym "=") C.binding)
  | Sym "|" -> Some (P.infix_seq ~sep:(tok, Prec.pipe) C.alt)
  | Sym ":" -> Some (P.infix_binary Prec.colon tok C.constraint')
  | Sym "->" -> Some (P.infix_right_binary Prec.arrow tok C.arrow)
  | Lower "as" ->
    Some (P.infix_binary Prec.as' tok (fun a b -> Shaper.shape "as" [ a; b ]))
  | Sym "." -> Some (parse_infix_dot, Prec.dot)
  | Sym "!" -> Some (macro_call, Prec.excl)
  | Sym "?" ->
    Some (P.postfix_unary Prec.dot tok (fun a -> Shaper.shape "?" [ a ]))
  | Sym s when check_is_operator_char s.[0] ->
    let precedence = Prec.get s in
    let rule =
      P.infix_binary precedence tok (fun a b -> C.apply (Shaper.sym s) [ a; b ])
    in
    Some rule
  | _ -> Shaper_parser.infix tok

let grammar =
  G.make
    ~default_infix:(P.parse_infix_juxt ~precedence:Prec.juxt Shaper.seq)
    ~prefix ~infix "fold"

let parse l : C.t =
  try P.run grammar l
  with exn ->
    (* FIXME!!! *)
    Fmt.epr "syntax error %a:@." Location.print_loc (Obj.magic (L.loc l));
    raise exn

let parse_chan ?file_name chan : C.t =
  let l = L.for_channel ?file_name chan in
  parse l

let parse_string str : C.t =
  let l = L.for_string str in
  parse l
