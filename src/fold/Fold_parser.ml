module G = Shaper_parser.Grammar
module L = Shaper_parser.Lexer
module S = Shaper
module P = Shaper_parser.Parser
module C = Fold_ast.Cons

type fl = Shaper.syntax

let ( let* ) = P.( let* )

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

let _shape_ = function
  | S.Seq (None, S.Ident (S.Lower x) :: args) -> C.shape x args
  | x ->
    Fmt.epr "shape: %a@." S.dump x;
    failwith "invalid shape syntax"

let shape (left : fl) g l =
  L.drop (L.Sym "!") l;
  match left with
  (* | Ident (Lower "calc") -> *)
  (* let x = P.parse_prefix Shaper_calc.grammar l in *)
  (* Result.map C.int x *)
  (* | Ident (Lower "xml") -> P.parse_prefix xml_grammar l *)
  | Ident (Lower kwd) -> (
    let* args = P.parse g l in
    match args with
    | S.Seq (None, items) -> Ok (S.shape kwd items)
    | _ -> Ok (S.shape kwd [ args ])
  )
  (* TODO: classify left in error *)
  | _ -> failwith "invalid macro call form, must be kwd!"

let prefix (tok : L.token) =
  match tok with
  (* | Sym "#" -> Some (P.prefix_unary ~precedence:Fold_precedence.juxt tok shape) *)
  (* beats: `,` `=`; beaten by: `;` *)
  | Lower (("fn" | "if" | "match" | "quote" | "unquote") as kwd) ->
    Some
      (P.prefix_unary ~precedence:Fold_precedence.juxt tok (function
        | S.Seq (None, items) -> C.shape kwd items
        | x -> C.shape kwd [ x ]
        )
        )
  | Lower (("let" | "module" | "open") as kwd) ->
    Some
      (P.prefix_unary ~precedence:Fold_precedence.item tok (function
        | S.Seq (None, items) -> C.shape kwd items
        | x -> C.shape kwd [ x ]
        )
        )
  | Lparen ->
    P.prefix_scope Lparen Rparen (function
      | None -> C.tuple []
      | Some (S.Seq (Some ",", items)) -> C.tuple items
      | Some x -> x
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

let infix_ (tok : L.token) =
  match tok with
  | Sym "`" -> None
  | Rparen | Rbrace | Rbracket -> Some P.infix_unbalanced
  | Lower "if"
  | Lower "when"
  | Lower "do"
  | Lower "end"
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
  | Sym "." -> Some (P.infix_binary 220 (L.Sym ".") C.dot)
  | Sym s when check_is_operator_char s.[0] ->
    let precedence = 100 + Fold_precedence.get s in
    let rule =
      P.infix_binary precedence tok (fun a b -> C.apply (Shaper.sym s) [ a; b ])
    in
    Some rule
  | _ -> None

let infix (tok : L.token) =
  match tok with
  | Rparen | Rbrace | Rbracket -> Some P.infix_unbalanced
  | Semi -> Some (P.infix_seq ~sep:(Semi, 10) (Shaper.seq ~sep:";"))
  | Sym "&" ->
    Some (P.infix_binary 19 tok (fun a b -> Shaper.shape "&" [ a; b ]))
  | Comma -> Some (P.infix_seq ~sep:(Comma, 20) (Shaper.seq ~sep:","))
  | Sym "=" -> Some (P.infix_binary 30 (L.Sym "=") C.binding)
  | Sym "|" -> Some (P.infix_seq ~sep:(tok, 40) C.alt)
  | Sym "->" -> Some (P.infix_binary 50 tok C.arrow)
  | Sym "!" -> Some (shape, 210)
  | Sym s when check_is_operator_char s.[0] ->
    let precedence = 100 + Fold_precedence.get s in
    let rule =
      P.infix_binary precedence tok (fun a b -> C.apply (Shaper.sym s) [ a; b ])
    in
    Some rule
  | _ -> None

let g =
  G.make ~default_infix:(P.parse_infix_juxt Shaper.seq) ~prefix ~infix "fold"

let parse l : C.t =
  let ast = P.run g l in
  ast
(* Fold_eval.eval ast *)

let parse_chan chan : C.t =
  let l = L.for_channel chan in
  parse l

let parse_string str : C.t =
  let l = L.for_string str in
  P.run g l
