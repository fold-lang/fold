
open Pure
open Lex


module Pratt = Pratt.Make(Syntax.Expr)


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


module DSL = struct
  let (~*) x   = Many x
  let (~+) x   = Some x
  let (~?) x   = Optional x
  let (||) x y = Alternative [x; y]
  let seq  xs  = Sequence xs
  let term x   = Terminal x
  let expr x   = Non_terminal x
  let e        = Epsilon
end



let rec to_parser self =
  let module P = Parser.Default in
  let (>>=) = P.(>>=) in
  let (<|>) = P.(<|>) in
  let (>>)  = P.(>>) in

  match self with
  | Epsilon ->
    P.pure []

  | Terminal x ->
    P.expect (Symbol x) >>= fun tok ->
    P.advance >> lazy (P.pure [tok])

  | Non_terminal x ->
    fail "not implemented"

  | Alternative [] ->
    P.pure []

  | Alternative [x] ->
    to_parser x

  | Alternative [x; y] ->
    to_parser x <|> to_parser y

  | Alternative (x::xs) ->
    List.fold_left (<|>) (to_parser x) (List.map to_parser xs)

  | Sequence [] ->
    P.pure []

  | Sequence [x] ->
    to_parser x

  | Sequence xs ->
    P.sequence (List.map to_parser xs) >>= fun xss -> P.pure (List.concat xss)

  | Optional x ->
    to_parser x <|> P.pure []

  | Many peg ->
    P.many (to_parser peg) >>= fun xss -> P.pure (List.concat xss)

  | Some peg ->
    P.some (to_parser peg) >>= fun xss -> P.pure (List.concat xss)


let terminals =
  let rec loop acc self =
    match self with
    | Epsilon        -> acc
    | Terminal     x -> x :: acc
    | Non_terminal _ -> acc
    | Sequence     xs
    | Alternative  xs -> List.fold_left (fun acc x -> loop acc x) acc xs
    | Optional     x
    | Many         x
    | Some         x -> loop acc x
  in
    loop []


let to_pratt self : Syntax.expr list Pratt.Parser.t =
  let module P = Pratt.Parser in
  let module G = Pratt.Grammar in
  let module S = Pratt.State in
  let (>>=) = P.(>>=) in
  let (<|>) = P.(<|>) in
  let (>>)  = P.(>>) in

  let rec loop = function
    | Epsilon ->
      P.pure []

    | Terminal x ->
      P.consume (Symbol x) >> lazy (P.pure [])

    | Non_terminal _ ->
      Pratt.expression >>= fun expr ->
      P.pure [expr]

    | Alternative [] ->
      P.pure []

    | Alternative [x] ->
      loop x

    | Alternative [x; y] ->
      loop x <|> loop y

    | Alternative (x::xs) ->
      List.fold_left (<|>) (loop x) (List.map loop xs)

    | Sequence [] ->
      P.pure []

    | Sequence [x] ->
      loop x

    | Sequence xs ->
      P.sequence (List.map loop xs) >>= fun xss -> P.pure (List.concat xss)

    | Optional x ->
      loop x <|> P.pure []

    | Many peg ->
      P.many (loop peg) >>= fun xss -> P.pure (List.concat xss)

    | Some peg ->
      P.some (loop peg) >>= fun xss -> P.pure (List.concat xss)
  in begin
    let ts = terminals self in
    P.modify begin fun s ->
      let g = List.fold_left
        (fun g str -> G.define (Symbol str) Pratt.Rule.delimiter g)
        S.(s.grammar)
        ts in
      { s with S.grammar = g }
    end >> lazy (loop self)
  end


