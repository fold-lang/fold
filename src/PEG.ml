
open Pure

let (>>=) = Parser.(>>=)
let (<|>) = Parser.(<|>)
let (>>)  = Parser.(>>)


type t =
  | Epsilon                 (* ε         *)
  | Terminal     of string  (* "x"       *)
  | Non_terminal of string  (* a         *)
  | Sequence     of t list  (* a b ...   *)
  | Alternative  of t list  (* a | b ... *)
  | Optional     of t       (* a?        *)
  | Many         of t       (* a*        *)
  | Some         of t       (* a+        *)


let rec to_string self =
  match self with
  | Epsilon         -> "ε"
  | Terminal     x  -> "\"" ^ x ^ "\""
  | Non_terminal x  -> x
  | Sequence     xs -> "(" ^ String.concat " "   (List.map to_string xs) ^ ")"
  | Alternative  xs -> "(" ^ String.concat " | " (List.map to_string xs) ^ ")"
  | Optional     x  -> to_string x ^ "?"
  | Many         x  -> to_string x ^ "*"
  | Some         x  -> to_string x ^ "+"


let rec parse self : 'a list Parser.t =
  match self with
  | Epsilon ->
    Parser.pure []

  | Terminal x ->
    Parser.expect (Lex.Symbol x) >>= fun tok ->
    Parser.advance >> lazy (Parser.pure [tok])

  | Alternative [] ->
    Parser.pure []

  | Alternative [x] ->
    parse x

  | Alternative [x; y] ->
    parse x <|> parse y

  | Alternative (x::xs) ->
    List.fold_left (<|>) (parse x) (List.map parse xs)

  | Sequence [] ->
    Parser.pure []

  | Sequence [x] ->
    parse x

  | Sequence xs ->
    Parser.sequence (List.map parse xs) >>= fun xss -> Parser.pure (List.concat xss)

  | Optional x ->
    parse x <|> Parser.pure []

  | Many peg ->
    Parser.many (parse peg) >>= fun xss -> Parser.pure (List.concat xss)

  | Some peg ->
    Parser.some (parse peg) >>= fun xss -> Parser.pure (List.concat xss)

