open Pure
open Base
open Lex


module type Input = sig
  type t
  type item

  val item_to_string : item -> string

  val next : t -> (item * t) option
end


module Make(Input : Input) = struct
  type error =
    | Empty
    | Unexpected_end   of { expected : Input.item }
    | Unexpected_token of { expected : Input.item; actual : Input.item }
    | Input_leftover   of Input.t
    | Failed_satisfy   of Input.item


  let error_to_string e =
    match e with
    | Empty -> "empty"
    | Unexpected_end { expected } ->
      "end of input while expecting `%s`" % Input.item_to_string expected
    | Unexpected_token { expected; actual } ->
      "expected `%s` but got `%s`" % (Input.item_to_string expected, Input.item_to_string actual)
    | Failed_satisfy token ->
      "token `%s` did not satisfy predicate" % Input.item_to_string token
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


  let token =
    get >>= fun state ->
    match Input.next state with
    | Some (x, _) -> pure x
    | None -> empty


  let next =
    get >>= fun state ->
    match Input.next state with
    | Some (x, rest) -> put rest >> lazy (pure x)
    | None -> empty


  let satisfy predicate =
    next >>= fun t ->
    if predicate t then pure t
    else error (Failed_satisfy t)


  let expect expected =
    get >>= fun state ->
    match Input.next state with
    | Some (actual, _) when actual = expected -> pure actual
    | Some (actual, _) -> error (Unexpected_token { expected; actual })
    | None -> error (Unexpected_end { expected })


  let advance =
    get >>= fun state ->
    match Input.next state with
    | Some (_, state') -> put state'
    | None -> empty


  let consume tok =
    expect tok >> lazy advance


  let exactly x = expect x >>= fun x -> advance >> lazy (pure x)


  let one_of list =
    satisfy (fun x -> List.mem x list)


  let none_of list =
    satisfy (fun x -> not (List.mem x list))
end

