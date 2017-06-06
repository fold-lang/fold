open Pure
open Lex


(**
 * Structurally a parser is a function which takes an input stream of
 * characters and yields a parse tree by applying the parser logic over
 * tokens to build up a composite data structure for the AST.
 *
 * Running the function will result in traversing the token stream yielding a
 * value of type [a] that usually represents the AST for the parsed
 * expression.
 *)

module type Input = sig
  type 'a t
  type item

  val current : 'a t -> item option
  val advance : 'a t -> 'a t
end


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
  (** [combine p1 p2] first parses [p1] and then [p2] returning a pair with
      corresponding results. *)

  val with_default : 'a -> ('a, 'x) t -> ('a, 'x) t
  (** [with_default default p] runs the parser [p] returning the default value in
      case it fails. *)

  val optional : ('a, 'x) t -> (unit, 'x) t
  (** [optional p] tries to optionally parse the input with parser [p] without
      returning its output. *)

  (* val parse : 'a t -> state -> ('a, string) Result.t *)
  (** [parse p s] runs the parser [p] with input state [s] producing a
      result of type [a] or an [error]. *)

  val error : error -> ('a, 'x) t
  (** [error e] is a parser that always fails with error [e]. *)

  val token : (token, 'x) t
  (** [token] is the current token in the input state. *)

  val consume : token -> (unit, 'x) t
  (** [advance tok] checks if the current token is equal to [tok] and advances
      the parser to the next token, or fails tokens are different. *)

  val expect : token -> (token, 'x) t
  (** [expect tok] checks if the current token is equal to [tok] failing if
      tokens are different. *)

  val advance : (unit, 'x) t
  (** [advance] advances the parser to the next token. *)

  val satisfy : (token -> bool) -> (token, 'x) t
  (** [satisfy test] is a parser that returns the current input token if it
      satisfies [test] predicate or fails otherwise. *)

  val exactly : token -> (token, 'x) t
  (** [exactly token] parses *exactly* [token]. *)

  val any : (token, 'x) t
  (** [any] is a parser that accepts any input token. *)

  val one_of : token list -> (token, 'x) t
  (** [one_of tokens] parses any token present in list [tokens]. *)

  val none_of : token list -> (token, 'x) t
  (** [none_of tokens] parses any token *not* present in list [tokens]. *)
end


module Make(Token : Printable)(Input : Input with type item = Token.t) :
  Type with type token = Token.t
        and type 'a state = 'a Input.t

