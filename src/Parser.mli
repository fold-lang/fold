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


include StateT
  with type state = Token.t Iter.t

include Functor
  with type 'a t := 'a t

include Applicative
  with type 'a t := 'a t

include Alternative
  with type 'a t := 'a t

val combine : 'a t -> 'b t -> ('a * 'b) t
(** [combine p1 p2] first parses [p1] and then [p2] returning a pair with
    corresponding results. *)

val parse : 'a t -> state -> ('a, string) Result.t
(** [parse p s] runs the parser [p] with input state [s] producing a
    result of type [a] or an [error]. *)

val error : string -> 'a t
(** [error msg] is a parser that always fails with error message [msg]. *)

val next : Token.t t
(** [next] parser the next token from the input state. *)

val satisfy : (Token.t -> bool) -> Token.t t
(** [satisfy test] is a parser that returns the current input token if it
    satisfies [test] predicate or fails otherwise. *)

val exactly : Token.t -> Token.t t
(** [exactly token] parses *exactly* [token]. *)

val one_of : Token.t list -> Token.t t
(** [one_of tokens] parses any token present in list [tokens]. *)

val none_of : Token.t list -> Token.t t
(** [none_of tokens] parses any token *not* present in list [tokens]. *)

val range : char -> char -> Token.t t
(** [range low high] parses any character token in the (inclusive) range
    defined by [low] and [high]. *)

