(*
 * Pratt 0.1
 * Copyright (c) 2017 Rizo Isrof. All rights reserved.
 *
 * Distributed under the ISC license, see LICENSE file.
 *)

(** Pratt is a simple top-down precedence parser.

    The grammar is defined as a set of rules that are matched on tokens. Input
    is processed linearly with an iterator. The parser searches for rules that
    match the current token based on its position (prefix or infix). Rules store
    the matching token, the precedence (for infix rules) and the parser that
    builds the final Abstract Syntax Tree (AST) object. *)

module type Lexer = sig
  type t
  type token

  val pp_token : Format.formatter -> token -> unit
  val compare_token : token -> token -> int
  val next : t -> token option
  val pick : t -> token option
end

module Make (Lexer : Lexer) : sig
  type token = Lexer.token
  (** The type of tokens to be parsed. *)

  type error =
    | Unexpected of { expected : token option; actual : token option }
    | Invalid_infix of token
    | Invalid_prefix of token
    | Zero  (** The type of errors for tokens of type ['a]. *)

  val unexpected_token : ?expected:token -> token -> error
  (** [unexpected_token ?expected t] is
      [Unexpected {actual = Some t;
       expected}]. *)

  val unexpected_end : ?expected:token -> unit -> error
  (** [unexpected_end ?expected ()] is [Unexpected {actual = None; expected}]. *)

  val invalid_prefix : token -> error
  (** [invalid_prefix t] is [Invalid_prefix t]. *)

  val invalid_infix : token -> error
  (** [invalid_infix t] is [Invalid_infix t]. *)

  val error_to_string : error -> string
  (** [error_to_string token_pp e] is a human-readable representation of [e]. *)

  val pp_error : Format.formatter -> error -> unit
  (** [pp_error token_pp] is a pretty printer for values of type [e] and
      contained tokens. *)

  (** {1:parser Parser} *)

  type 'a parser
  (** A parser producing values of type ['a]. *)

  (** {2:parser-monad Monad Instance} *)

  val return : 'a -> 'a parser
  (** [return x] is a parser producing [x] as a value regardless of the input. *)

  val ( >>= ) : 'a parser -> ('a -> 'b parser) -> 'b parser
  (** [p >>= f] is a parser returned by [f] after applying [f] to the result of
      [p]. *)

  val error : error -> 'a parser
  (** [error e] is a parser that fails with the error [e] without consuming any
      input *)

  val zero : 'a parser
  (** [zero] is a parser that fails without consuming any input. *)

  val ( <|> ) : 'a parser -> 'a parser -> 'a parser
  (** [p <|> q] is a choice combinator. Parser [p] is first applied, if it
      succeeds its value is returned. If [p] fails
      {e without consuming any input}, [q] is tried. *)

  (** {1:combinators Parsing Combinators} *)

  val default : 'a -> 'a parser -> 'a parser
  (** [default x p] runs the parser [p] producing the default [x] value if it
      fails. *)

  val combine : 'a parser -> 'b parser -> ('a * 'b) parser
  (** [combine p q] first parses [p] and then [q] returning a pair with
      corresponding results. *)

  val many : 'a parser -> 'a list parser
  (** [many p] applies the parser [p] zero or more times. Returns a list of the
      returned values of [p]. *)

  val some : 'a parser -> ('a * 'a list) parser
  (** [some p] applies the parser [p] one or more times. Returns the guaranteed
      first value and a potentially empty list of values parsed by [p]. *)

  val optional : 'a parser -> unit parser
  (** [optional p] tries to optionally parse the input with parser [p] without
      returning its output. *)

  val current : token parser
  (** [current] is the parser that produces the current token as the result. *)

  val next : token parser
  (** [next] returns the current token advances and advances to the next one. *)

  val expect : token -> token parser
  (** [expect token] checks if the current token in the input is equal to
      [token] failing if it is not. *)

  val advance : unit parser
  (** [advance] advances the parser to the next token. *)

  val consume : token -> unit parser
  (** [consume token] checks if the current token is equal to [token] and
      advances the parser to the next token, or fails if they are different. *)

  val satisfy : (token -> bool) -> token parser
  (** [satisfy test] is a parser that returns the current input token if it
      satisfies [test] predicate or fails otherwise. *)

  val exactly : token -> token parser
  (** [exactly token] parses *exactly* the given [token]. *)

  val any : token parser
  (** [any] is a parser that accepts any input token. *)

  val from : token list -> token parser
  (** [from tokens] parses any token from [tokens] list. *)

  val none : token list -> token parser
  (** [none tokens] parses any token *not* present in [tokens] list. *)

  val range : ?compare:(token -> token -> int) -> token -> token -> token parser
  (** [range ?compare s e] parses any token in the range defined by [s] and [e].
      Optionally a custom [compare] function can be supplied. *)

  val choice : 'a parser list -> 'a parser
  (** [choice ps] is a parser that tries all the parsers in [ps] until one of
      them succeeds. *)

  val guard : bool -> unit parser
  val when' : bool -> unit parser -> unit parser
  val unless : bool -> unit parser -> unit parser

  val many_while : (token -> bool) -> 'a parser -> 'a list parser
  (** [many_while test p] repeatedly runs the parser [p] while the input token
      satisfies [test]. Stops when the token fails the [test] or if the input is
      empty. *)

  val some_while : (token -> bool) -> 'a parser -> ('a * 'a list) parser
  (** [some_while test p] is just like [many_while] except it parser at least
      one ['a]. *)

  type 'a rule
  (** The type for parsing rules for tokens of type ['t] producing results of
      type ['a]. *)

  (** {1:grammar Grammar} *)

  type 'a grammar
  (** Grammar type holding parsing rules for tokens of type ['t] and parsed
      values of type ['a]. *)

  module Grammar : sig
    type 'a t = 'a grammar

    val has_null : token -> 'a grammar -> bool
    val has_left : token -> 'a grammar -> bool
    val dump : (Format.formatter -> token -> unit) -> 'a t -> unit
    val new_scope : 'a t -> 'a t
    val pop_scope : 'a t -> 'a t
    val add : 'a rule -> 'a t -> 'a t
  end

  val nud : 'a grammar -> 'a parser
  val led : int -> 'a grammar -> 'a -> 'a parser

  (** {1:rules Rules} *)

  val rule : token -> ('a grammar -> 'a parser) -> 'a rule
  (** [rule t p] is a rule with parser [p] for prefix token [t]. *)

  val term : ('a grammar -> 'a parser) -> 'a rule
  (** [term p] is a parser for literals or variables. *)

  val infix : int -> token -> ('a -> 'a -> 'a) -> 'a rule
  (** [infix precedence token f] is a rule that parses infix occurrences of
      [token] with given [precedence] applying [f] to the {e lhs} and {e rhs}
      expressions. This rule is left-associative. *)

  val infixr : int -> token -> ('a -> 'a -> 'a) -> 'a rule
  (** [infixr precedence token f] is like {!infix} except it is
      right-associative. *)

  val prefix : token -> ('a -> 'a) -> 'a rule
  (** [prefix token f] is a rule that parses prefix occurrences of [token]
      applying [f] to the {e rhs} expression. *)

  val postfix : int -> token -> ('a -> 'a) -> 'a rule
  (** [postfix precedence token f] is a rule that parses postfix occurrences of
      [token] with given [precedence] applying [f] to the {e lhs} expression. *)

  val between : token -> token -> ('a -> 'a) -> 'a rule

  val delimiter : token -> 'a rule
  (** [delimiter token] is a rule that parses a delimiter [token]. *)

  val binary : ('a -> 'a -> 'a) -> 'a grammar -> 'a -> 'a parser
  val unary : ('a -> 'a) -> 'a grammar -> 'a parser
  val null : token -> ('a grammar -> 'a parser) -> 'a rule
  val left : int -> token -> ('a grammar -> 'a -> 'a parser) -> 'a rule

  (** {1:parsing Parsing} *)

  val grammar : 'a rule list -> 'a grammar
  (** [grammar rules] is a grammar for a language constructed with [rules]. *)

  val parse : ?precedence:int -> 'a grammar -> 'a parser
  (** [parse ?precedence g] is the parser for the grammar [g] starting with
      binding power [precedence]. *)

  val parse_many : 'a grammar -> 'a list parser
  val parse_some : 'a grammar -> ('a * 'a list) parser

  val run : 'a parser -> Lexer.t -> ('a, error) result
  (** [run p input] is the result of running the parser [p] with the given
      [input]. The parsed value is produced with the remaining input. *)
end
