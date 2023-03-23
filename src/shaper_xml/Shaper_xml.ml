module G = Shaper_parser.Grammar
module L = Shaper_parser.Lexer
module P = Shaper_parser.Parser

let ( let* ) = P.( let* )

type xml =
  | Elem of
      { tag : string; attrs : (string * string) list; children : xml list }
  | Data of string

type 'a eval =
  { elem : tag:string -> attrs:(string * string) list -> 'a list -> 'a
  ; data : string -> 'a
  }

let concrete_eval =
  { elem = (fun ~tag ~attrs children -> Elem { tag; attrs; children })
  ; data = (fun data -> Data data)
  }

let rec pp f xml =
  match xml with
  | Data x -> Fmt.Dump.string f x
  | Elem { tag; attrs = _; children = [] } -> Fmt.pf f "<%s />" tag
  | Elem { tag; attrs = _; children } ->
    Fmt.pf f "@[<hov2><%s>%a</%s>@]" tag (Fmt.list ~sep:Fmt.cut pp) children tag

let parse_data ~eval _g l =
  match L.pick l with
  | L.Int x ->
    L.move l;
    Ok (eval.data (string_of_int x))
  | L.String x ->
    L.move l;
    Ok (eval.data x)
  | t -> Fmt.failwith "xml: not constant: %a" L.pp_token t

let parse_tag ~eval g l =
  L.drop (L.Sym "<") l;
  match L.pick l with
  | L.Lower tag ->
    L.move l;
    let* () = P.consume (L.Sym ">") g l in
    let* children =
      P.until
        (fun tok -> not (G.has_infix g tok || L.is_eof tok))
        (P.parse_prefix g) l
    in
    let* () = P.consume (L.Sym "</") g l in
    let* () = P.consume (L.Lower tag) g l in
    let* () = P.consume (L.Sym ">") g l in
    Ok (eval.elem ~tag ~attrs:[] children)
  | tok -> P.unexpected_token ~ctx:"tag" tok

let make_rules ~eval =
  [ P.Prefix (L.Sym "<", parse_tag ~eval); P.delimiter (L.Sym "</") ]

let make_grammar ~eval =
  let default_prefix = parse_data ~eval in
  let rules = make_rules ~eval in
  G.make ~default_prefix ~name:"xml" rules

let grammar = make_grammar ~eval:concrete_eval
