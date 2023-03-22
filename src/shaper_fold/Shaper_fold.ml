module S = Shaper.V03
module G = Shaper_parser.Grammar
module L = Shaper_parser.Lexer
module P = Shaper_parser.Parser

let ( let* ) = P.( let* )

let default_prefix _g l =
  match L.pick l with
  | L.Int x ->
    L.move l;
    Ok x
  | t -> Fmt.failwith "calc: not constant: %a" L.pp_token t

let rules =
  [ P.prefix (L.Sym "..") (fun a -> S.form ".." [ a ])
  ; P.prefix (L.Lower "open") (fun a -> S.form "open" [ a ])
  ; P.prefix ~precedence:2 (L.Lower "let") (fun a -> S.form "let" [ a ])
  ; P.prefix ~precedence:2 (L.Lower "val") (fun a -> S.form "val" [ a ])
  ; P.invalid_infix_rule (L.Lower "let")
  ; P.invalid_infix_rule (L.Lower "val")
  ; P.infix 10 (L.Sym "=") (fun a b -> S.form "=" [ a; b ])
  ; P.infix 30 (L.Sym "|") (fun a b -> S.form "|" [ a; b ])
  ; P.infix 40 (L.Sym "->") (fun a b -> S.form "->" [ a; b ])
  ; P.infix 80 (L.Sym ".") (fun a b -> S.form "." [ a; b ])
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
    let seq = S.seq [ S.sym s; x1; x2 ] in
    Ok seq
  | _ -> P.juxt S.seq x1 g l

let g =
  G.make ~default_prefix:Shaper_parser.const ~default_infix ~name:"fold" rules
  |> G.push_scope (G.scope_of_list Shaper_parser.rules)

let parse chan =
  let l = L.for_channel chan in
  P.run g l
