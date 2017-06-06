open Pure
open Base
open Lex


module type Input = sig
  type 'a t
  type item

  val current : 'a t -> item option
  val advance : 'a t -> 'a t
end


(* module Input : sig *)
(*   module String  : Input *)
(*   module Channel : Input *)
(* end *)


module type Type = sig
  type token

  type error =
    | Empty
    | Unexpected_end   of { expected : token }
    | Unexpected_token of { expected : token; actual : token }
    | Failed_satisfy   of token
    | With_message     of string

  val error_to_string : error -> string

  include State1T
      with type 'a monad = ('a, error) Result.t

  include Functor2
    with type ('a, 'x) t := ('a, 'x) t

  include Applicative2
    with type ('a, 'x) t := ('a, 'x) t

  include Alternative2
    with type ('a, 'x) t := ('a, 'x) t

  val combine : ('a, 'x) t -> ('b, 'x) t -> ('a * 'b, 'x) t

  val with_default : 'a -> ('a, 'x) t -> ('a, 'x) t

  val optional : ('a, 'x) t -> (unit, 'x) t

  val error : error -> ('a, 'x) t

  val token : (token, 'x) t

  val consume : token -> (unit, 'x) t

  val expect : token -> (token, 'x) t

  val advance : (unit, 'x) t

  val satisfy : (token -> bool) -> (token, 'x) t

  val exactly : token -> (token, 'x) t

  val any : (token, 'x) t

  val one_of : token list -> (token, 'x) t

  val none_of : token list -> (token, 'x) t
end


module Make(Token : Printable)(Input : Input with type item = Token.t) = struct
  type token = Token.t

  type error =
    | Empty
    | Unexpected_end   of { expected : token }
    | Unexpected_token of { expected : token; actual : token }
    | Failed_satisfy   of token
    | With_message     of string


  let error_to_string e =
    match e with
    | Empty -> "empty"

    (* FIXME: Not used in practice with curren Input interface.
       Should be refactored to produce this constructor. *)
    | Unexpected_end { expected } ->
      "expected `%s` but input terminated" % Token.show expected

    | Unexpected_token { expected; actual } when Token.show actual = "__eof__" ->
      "expected `%s` but input terminated" % Token.show expected

    | Unexpected_token { expected; actual } ->
      "expected `%s` but got `%s`" % (Token.show expected, Token.show actual)
    | Failed_satisfy token ->
      "token `%s` did not satisfy predicate" % Token.show token
    | With_message msg ->
      msg


  module Result =
    Result.Of_error(struct type t = error end)


  module State1T =
    State1T(Input)(Result)


  module Functor2 =
    Functor2.Of_monad(State1T)


  module Applicative2 =
    Applicative2.Of_monad(State1T)


  include State1T
  include Functor2
  include Applicative2


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
    match Input.current state with
    | Some x -> pure x
    | None -> empty


  let satisfy pred =
    token >>= fun t ->
    if pred t then pure t
    else error (Failed_satisfy t)


  let expect expected =
    get >>= fun state ->
    match Input.current state with
    | Some actual when actual = expected -> pure actual
    | Some actual -> error (Unexpected_token { expected; actual })
    | None -> error (Unexpected_end { expected })


  let advance =
    get >>= fun state ->
    put (Input.advance state)


  let consume tok =
    expect tok >> lazy advance


  let exactly x = expect x >>= fun x -> advance >> lazy (pure x)


  let any = satisfy (const true)


  let one_of list =
    satisfy (fun x -> List.mem x list)


  let none_of list =
    satisfy (fun x -> not (List.mem x list))
end

