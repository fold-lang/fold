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
  type t
  type item

  val item_to_string : item -> string

  val next : t -> (item * t) option
end


module Make(Input : Input) : sig
  type error =
    | Empty
    | Unexpected_end   of { expected : Input.item }
    | Unexpected_token of { expected : Input.item; actual : Input.item }
    | Input_leftover   of Input.t
    | Failed_satisfy   of Input.item

  val error_to_string : error -> string

  include StateT
    with type state = Input.t
     and type 'a monad = ('a, error) Result.t

  include Functor
    with type 'a t := 'a t

  include Applicative
    with type 'a t := 'a t

  include Alternative
    with type 'a t := 'a t


  val combine : 'a t -> 'b t -> ('a * 'b) t
  (** [combine p1 p2] first parses [p1] and then [p2] returning a pair with
      corresponding results. *)

  val with_default : 'a -> 'a t -> 'a t
  (** [with_default default p] runs the parser [p] returning the default value in
      case it fails. *)

  val optional : 'a t -> unit t
  (** [optional p] tries to optionally parse the input with parser [p] without
      returning its output. *)

  (* val parse : 'a t -> state -> ('a, string) Result.t *)
  (** [parse p s] runs the parser [p] with input state [s] producing a
      result of type [a] or an [error]. *)

  val error : error -> 'a t
  (** [error e] is a parser that always fails with error [e]. *)

  val token : Input.item t
  (** [token] is the current token in the input state. *)

  val consume : Input.item -> unit t
  (** [advance tok] checks if the current token is equal to [tok] and advances
      the parser to the next token, or fails tokens are different. *)

  val expect : Input.item -> Input.item t
  (** [expect tok] checks if the current token is equal to [tok] failing if
      tokens are different. *)

  val advance : unit t
  (** [advance] advances the parser to the next token. *)

  val satisfy : (Input.item -> bool) -> Input.item t
  (** [satisfy test] is a parser that returns the current input token if it
      satisfies [test] predicate or fails otherwise. *)

  val exactly : Input.item -> Input.item t
  (** [exactly token] parses *exactly* [token]. *)

  val one_of : Input.item list -> Input.item t
  (** [one_of tokens] parses any token present in list [tokens]. *)

  val none_of : Input.item list -> Input.item t
  (** [none_of tokens] parses any token *not* present in list [tokens]. *)
end



