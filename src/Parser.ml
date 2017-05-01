open Pure
open Base
open Lex


module type Input = sig
  type t
  type item

  val current : t -> item option
  val advance : t -> t
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

  include StateT
    with type 'a monad = ('a, error) Result.t

  include Functor
    with type 'a t := 'a t

  include Applicative
    with type 'a t := 'a t

  include Alternative
    with type 'a t := 'a t

  val combine : 'a t -> 'b t -> ('a * 'b) t

  val with_default : 'a -> 'a t -> 'a t

  val optional : 'a t -> unit t

  val error : error -> 'a t

  val token : token t

  val consume : token -> unit t

  val expect : token -> token t

  val advance : unit t

  val satisfy : (token -> bool) -> token t

  val exactly : token -> token t

  val any : token t

  val one_of : token list -> token t

  val none_of : token list -> token t
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
    | Unexpected_end { expected } ->
      "end of input while expecting `%s`" % Token.show expected
    | Unexpected_token { expected; actual } ->
      "expected `%s` but got `%s`" % (Token.show expected, Token.show actual)
    | Failed_satisfy token ->
      "token `%s` did not satisfy predicate" % Token.show token
    | With_message msg ->
      msg


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



module Default_input = struct
  type t = Token.t list
  type item = Token.t

  let current l = Option.catch (fun () -> List.hd l)
  let advance l = List.tl l
end


module Default = Make(Token)(Default_input)


