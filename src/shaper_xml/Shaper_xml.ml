module L = Shaper_parser.Lexer
module P = Shaper_parser.Parser
module G = Shaper_parser.Parser.Grammar

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

let parse_tag ~eval g l =
  let* () = P.consume (L.Sym "<") l in
  match L.pick l with
  | L.Lower tag ->
    L.move l;
    let* () = P.consume (L.Sym ">") l in
    let* children =
      P.until
        (fun tok -> not (G.has_infix tok g || L.is_eof tok))
        (P.parse_prefix g) l
    in
    let* () = P.consume (L.Sym "</") l in
    let* () = P.consume (L.Lower tag) l in
    let* () = P.consume (L.Sym ">") l in
    Ok (eval.elem ~tag ~attrs:[] children)
  | tok -> P.unexpected_token ~ctx:"tag" tok

let make_prefix ~eval (tok : L.token) =
  match tok with
  | Sym "<" -> Some (parse_tag ~eval)
  | L.Int x -> Some (P.const (eval.data (string_of_int x)))
  | L.String x -> Some (P.const (eval.data x))
  | _ -> None

let infix (tok : L.token) =
  match tok with
  | Sym "</" -> Some P.infix_delimiter
  | _ -> None

let make_grammar ~eval =
  let prefix = make_prefix ~eval in
  G.make ~prefix ~infix "xml"

let grammar = make_grammar ~eval:concrete_eval
