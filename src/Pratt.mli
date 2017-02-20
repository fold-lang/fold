open Pure
open Lex
open Syntax

module R : Monad.Base
module M : Map.S


module rec State : Type

and Env : sig
  type t

  val lookup_prefix     : string -> t -> Parser.prefix option
  val lookup_infix      : string -> t -> Parser.infix  option
  val lookup_precedence : string -> t -> int           option

  val define_prefix     : string -> Parser.prefix -> t -> t
  val define_infix      : string -> Parser.infix  -> t -> t
  val define_precedence : string -> int           -> t -> t
end

and Parser : sig
  include StateT

  type prefix = expr t
  type infix  = expr -> expr t
end

val parse : Lexer.t -> (expr, string) result
val parse_string : expr Parser.t -> string -> (expr, string) result


