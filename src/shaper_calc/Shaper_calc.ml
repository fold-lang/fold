module G = Shaper_parser.Grammar
module L = Shaper_parser.Lexer
module P = Shaper_parser.Parser

let rec fac = function
  | 0 | 1 -> 1
  | n -> n * fac (n - 1)

let default_prefix _g l =
  match L.pick l with
  | L.Int x ->
    L.move l;
    Ok x
  | t -> Fmt.failwith "not an atom: %a" L.pp_token t

let grammar =
  G.make ~default_prefix ~name:"clac"
    [ P.prefix (L.Sym "+") (fun x -> x)
    ; P.prefix (L.Sym "-") (fun x -> -x)
    ; P.infix 30 (L.Sym "+") ( + )
    ; P.infix 30 (L.Sym "-") ( - )
    ; P.infix 40 (L.Sym "*") ( * )
    ; P.infix 40 (L.Sym "/") ( / )
    ; P.postfix 70 (L.Sym "!") fac
    ; P.between L.Lparen L.Rparen (fun x -> x)
    ]
