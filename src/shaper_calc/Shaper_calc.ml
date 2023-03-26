module L = Shaper_parser.Lexer
module P = Shaper_parser.Parser
module G = Shaper_parser.Parser.Grammar

let some = Option.some

let rec fac = function
  | 0 | 1 -> 1
  | n -> n * fac (n - 1)

let default_prefix _g l =
  match L.pick l with
  | L.Int x ->
    L.move l;
    Ok x
  | t -> Fmt.failwith "calc: not constant: %a" L.pp_token t

let prefix (tok : L.token) =
  match tok with
  | Int x -> Some (P.const x)
  | Sym "+" -> Some (P.prefix_unary tok (fun x -> x))
  | Sym "-" -> Some (P.prefix_unary tok (fun x -> -x))
  | Lparen -> Some (P.between Lparen Rparen (fun x -> x))
  | _ -> None

let infix (tok : L.token) =
  match tok with
  | Rparen -> Some P.infix_delimiter
  | Sym "+" -> Some (P.infix_binary 30 tok ( + ))
  | Sym "-" -> Some (P.infix_binary 30 tok ( - ))
  | Sym "*" -> Some (P.infix_binary 40 tok ( * ))
  | Sym "/" -> Some (P.infix_binary 40 tok ( / ))
  | Semi -> Some (P.infix_right_binary 50 tok (fun _ x -> x))
  | Sym "!" -> Some (P.postfix_unary 70 tok fac)
  | _ -> None

let grammar = G.make ~prefix ~infix "calc"
