
open Pure
open Base
open Lex

module M = Map.Make(Token)


type error =
  | Empty
  | Unexpected_end   of { expected : token }
  | Unexpected_token of { expected : token; actual : token }
  | Failed_satisfy   of token
  | With_message     of string

let error_to_string e =
  match e with
  | Empty -> "empty"

  | Unexpected_end { expected } ->
    "expected `%s` but input terminated" % Token.show expected

  | Unexpected_token { expected; actual } ->
    "expected `%s` but got `%s`" % (Token.show expected, Token.show actual)

  | Failed_satisfy token ->
    "token `%s` did not satisfy predicate" % Token.show token

  | With_message msg ->
    msg


type ('a, 'state) parser = 'state -> ('a * 'state, error) result

type ('a, 'state) prefix = ('a, 'state) parser
type ('a, 'state) infix  = ('a -> ('a, 'state) parser) * int

type ('a, 'state) rule =
  | Prefix of Token.t * ('a, 'state) prefix
  | Infix  of Token.t * ('a, 'state) infix

type ('a, 'state) scope = {
  prefix : ('a, 'state) prefix M.t;
  infix  : ('a, 'state) infix  M.t;
}

type state =
  { lexer   : Lexer.t;
    token   : Token.t }

(* Monad *)
let pure x  = fun s -> Ok (x, s)

let (>>=) p f = fun s ->
    match p s with
    | Ok (x, s') -> (f x) s'
    | Error msg  -> Error msg


(* Alternative *)
let empty = fun _state -> Error Empty

let (<|>) p1 p2 = fun state ->
  match p1 state with
  | Ok value -> Ok value
  | Error _  -> p2 state

let get       = fun s -> Ok (s, s)
let put s     = fun _ -> Ok ((), s)
let zero      = fun s -> Ok ((), s)

let combine p1 p2 =
  p1 >>= fun x ->
  p2 >>= fun y -> pure (x, y)


let with_default default p =
  p <|> pure default


let optional p =
  p >>= (fun () -> pure ()) |> with_default ()


let rec many p =
  (p >>= fun x -> many p >>= fun xs -> pure (x :: xs))
  |> with_default []


let rec some p =
  combine p (many p) >>= fun (x, xs) -> pure (x :: xs)


let error e =
  fun _state -> Error e

let state f =
  get >>= fun s ->
  let (a, s') = f s in
  put s' >>= fun () -> pure a


let modify f =
  state (fun s -> ((), f s))


let satisfy pred =
  get >>= fun {token} ->
  if pred token then pure token
  else error (Failed_satisfy token)


let expect expected =
  get >>= fun {token} ->
  match token with
  | actual when actual = expected -> pure actual
  | actual when actual = Lex.eof -> error (Unexpected_end { expected })
  | actual -> error (Unexpected_token { expected; actual })


let advance = fun state ->
  modify (fun state -> { state with token = Lexer.read state.lexer })
    state

let consume tok =
  expect tok >>= fun _ -> advance


let exactly x =
  expect x >>= fun x -> advance >>= fun () -> pure x


let any = fun state ->
  satisfy (const true)
    state


let one_of list =
  satisfy (fun x -> List.mem x list)


let none_of list =
  satisfy (fun x -> not (List.mem x list))

