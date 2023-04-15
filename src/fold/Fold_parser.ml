module P = Pratt
module G = Pratt.Grammar
module L = Pratt.Lexer
module S = Shaper
module C = Fold_ast.Cons
module Prec = Fold_precedence
open Prelude

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
    | Semi -> Some (P.infix_seq ~sep:(Semi, Prec.semi) (Shaper.seq ~sep:";"))
    | Comma -> Some (P.infix_seq ~sep:(Comma, Prec.comma) (Shaper.seq ~sep:","))
    | _ -> None

  let grammar =
    G.make
      ~default_infix:(P.parse_infix_juxt Shaper.seq)
      ~prefix ~infix "shaper"

  let parse_string str : C.t =
    let l = L.for_string str in
    P.run grammar l
end

let ( let* ) = P.( let* )

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
      | Shaper.Scope ("(", Shaper.Seq (None, items), ")") -> items
      | Shaper.Scope _ -> [ arg ]
      | _ -> failwith "macro arguments must be scoped"
    in
    let shape = Shaper.shape ~loc kwd args in
    Ok shape
  | _ -> failwith "invalid macro call form, must be kwd!"

let prefix (tok : L.token) =
  match tok with
  | Lower
      ( ("fn" | "if" | "match" | "quote" | "unquote" | "for" | "while" | "try")
      as kwd
      ) ->
    Some
      (P.prefix_unary ~precedence:Fold_precedence.juxt tok (function
        | S.Seq (None, items) -> C.shape kwd items
        | x -> C.shape kwd [ x ]
        )
        )
  | Lower (("let" | "module" | "open" | "do" | "type") as kwd) ->
    Some
      (P.prefix_unary ~precedence:Fold_precedence.item tok (function
        | S.Seq (None, items) -> C.shape kwd items
        | x -> C.shape kwd [ x ]
        )
        )
  (* prefix tight *)
  | Sym (("#" | "~") as kwd) ->
    let rule g l =
      let* () = P.consume tok l in
      let* x = P.parse_prefix g l in
      Ok (S.shape kwd [ x ])
    in
    Some rule
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
      | "fn"
      | "module"
      | "open"
      | "val" ) -> Some P.infix_delimiter
  | Lower "if" ->
    Some (P.infix_binary 90 tok (fun a b -> Shaper.shape "_if_" [ a; b ]))
  | Lower (("to" | "downto") as kwd) ->
    Some (P.infix_binary 90 tok (fun a b -> Shaper.shape kwd [ a; b ]))
  | Sym "#" -> None
  | Sym "&" ->
    Some (P.infix_binary Prec.ampr tok (fun a b -> Shaper.shape "&" [ a; b ]))
  | Sym "=" -> Some (P.infix_binary Prec.equal (L.Sym "=") C.binding)
  | Sym "|" -> Some (P.infix_seq ~sep:(tok, Prec.pipe) C.alt)
  | Sym "->" -> Some (P.infix_binary Prec.arrow tok C.arrow)
  | Sym ":" -> Some (P.infix_binary Prec.colon tok C.constraint')
  | Sym "." -> Some (P.infix_binary Prec.dot tok C.dot)
  | Sym "!" -> Some (macro_call, Prec.excl)
  | Sym s when check_is_operator_char s.[0] ->
    let precedence = Prec.get s in
    let rule =
      P.infix_binary precedence tok (fun a b -> C.apply (Shaper.sym s) [ a; b ])
    in
    Some rule
  | _ -> Shaper_parser.infix tok

let grammar =
  G.make ~default_infix:(P.parse_infix_juxt Shaper.seq) ~prefix ~infix "fold"

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
