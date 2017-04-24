
open Lex
open Syntax

module P = Parser


module rec Grammar : sig
  type t
  (** The type of Pratt grammar.
      Includes prefix and infix rules with precendence. *)

  type prefix = expr Parser.t
  (** The type of prefix expression parsers. *)

  type infix = (expr -> expr Parser.t) * int
  (** The type of infix expression parsers (with precendence). *)

  val empty : t
  (** [empty] is a grammar without any rule definitions. *)

  val define_prefix : string -> prefix -> t -> t
  (** [define_prefix name rule g] defines a new prefix [rule] with [name]. *)

  val define_infix : string -> infix -> t -> t
  (** [define_infix name rule g] defines a new infix [rule] with [name]. *)

  val lookup_prefix : string -> t -> prefix option
  (** [lookup_prefix name g] is a prefix parsing rule defined as [name]. *)

  val lookup_infix : string -> t -> infix option
  (** [lookup_infix name g] is a infix parsing rule defined as [name]. *)
end

and State : sig
  (** State module holds the lexer and the grammar of the Pratt parser. *)

  type t = {
    lexer   : Lexer.t;
    grammar : Grammar.t;
    token   : Token.t option;
  }
  (** The type of Pratt state values. *)

  include P.Input with type t := t

  val init : ?grammar: Grammar.t -> lexer: Lexer.t -> unit -> t
  (** [init ?grammar ~lexer ()] is a new state with [lexer] and optional [grammar] *)
end

and Parser : P.Type with type state = State.t
                     and type token = Token.t
(** Module implementing the [Parser] interface with Pratt state. *)

val infix : int -> expr -> expr Parser.t
(** [infix lbp left] is an infix expression parser with previously parsed
    [left] expression and left binding power [lbp]. *)

val prefix : int -> expr Parser.t
(** [prefix lbp] is a prefix expression parser with left binding power [lbp]. *)

val expression : expr Parser.t
(** [expression] parser an expression, equivalent to [prefix 0]. *)

val run : expr Parser.t -> ?grammar: Grammar.t -> Lexer.t -> (expr, string) result
(** [run p ?grammar lexer] runs the parser reading from [lexer]. *)

val parse : ?grammar: Grammar.t -> Lexer.t -> (expr, string) result
(** [parse ?grammar lexer] parses an expression reading from [lexer]. *)

val invalid_infix  : expr -> expr Parser.t

val invalid_prefix : expr Parser.t

