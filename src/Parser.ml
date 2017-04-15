open Pure
open Base
open Lex


module Input = struct
  type t = Token.t Iter.t
end


type error =
  | Empty
  | Unexpected_end   of { expected : Token.t }
  | Unexpected_token of { expected : Token.t; actual : Token.t }
  | Input_leftover   of Input.t
  | Failed_satisfy   of Token.t

let error_to_string e =
  match e with
  | Empty -> "empty"
  | Unexpected_end { expected } ->
    "end of input while expecting `%s`" % Token.to_string expected
  | Unexpected_token { expected; actual } ->
    "expected `%s` but got `%s`" % (Token.to_string expected, Token.to_string actual)
  | Failed_satisfy token ->
    "token `%s` did not satisfy predicate" % Token.to_string token
  | Input_leftover _ ->
    "parser did not consume entire input"


module Result =
  Result.Of_error(struct type t = error end)


module StateT =
  StateT(Input)(Result)


module Functor =
  Functor.Of_monad(StateT)


module Applicative =
  Applicative.Of_monad(StateT)


include StateT
include Functor
include Applicative


(* Alternative instance *)
let empty = fun _state -> Error Empty

let (<|>) p1 p2 = fun state ->
  match p1 state with
  | Ok value -> Ok value
  | Error _  -> p2 state


let combine p1 p2 =
  p1 >>= fun x ->
  p2 >>= fun y -> pure (x, y)


let with_default default p =
  p <|> pure default


let optional p =
  p >> lazy (pure ()) |> with_default ()


let rec many p =
  (p >>= fun x -> many p >>= fun xs -> pure (x :: xs))
  |> with_default []


let rec some p =
  combine p (many p) >>= fun (x, xs) -> pure (x :: xs)


let error e =
  fun _state -> Error e


let next =
  get >>= fun state ->
  match Iter.view state with
  | Some (x, rest) -> put rest >> lazy (pure x)
  | None -> empty


let token =
  get >>= fun state ->
  match Iter.view state with
  | Some (x, _) -> pure x
  | None -> pure Token.eof


let satisfy predicate =
  token >>= fun t ->
  if predicate t then pure t
  else error (Failed_satisfy t)


let expect expected =
  token >>= function
  | tok when tok = Token.eof -> error (Unexpected_end   { expected })
  | tok when tok <> expected -> error (Unexpected_token { expected; actual = tok })
  | tok -> pure tok


let advance = modify Iter.tail


let consume tok =
  expect tok >> lazy advance



let exactly x = satisfy ((=) x)


let one_of list =
  satisfy (fun x -> List.mem x list)


let none_of list =
  satisfy (fun x -> not (List.mem x list))


let range low high =
  satisfy (fun x -> Char low <= x && x <= Char high)


