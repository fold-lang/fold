open Pure
open Base
open Lex


module Input = struct
  type t = Token.t Iter.t
end


module Result =
  Result.Of_error(String)


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
let empty = fun _state -> Error "empty"

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


let parse parser state =
  match parser state with
  | Ok (a, s) when Iter.is_empty s -> Ok a
  | Ok _leftover -> Error "parser did not consume entire input"
  | Error msg -> Error ("parsing error: " ^ msg)


let error msg =
  fun _state -> Error msg


let next =
  get >>= fun state ->
  match Iter.view state with
  | Some (x, rest) -> put rest >> lazy (pure x)
  | None -> error "empty input"


let satisfy predicate =
  next >>= fun token ->
  if predicate token then pure token
  else error ("token `%s` did not satisfy test" % Token.to_string token)


let exactly x = satisfy ((=) x)


let one_of list =
  satisfy (fun x -> List.mem x list)


let none_of list =
  satisfy (fun x -> not (List.mem x list))


let range low high =
  satisfy (fun x -> Char low <= x && x <= Char high)


