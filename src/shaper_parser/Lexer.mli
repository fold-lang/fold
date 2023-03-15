type token =
  | Int of int
  | Lower of string
  | Upper of string
  | Sym of string
  | Lbrace
  | Rbrace
  | Lparen
  | Rparen
  | Comma
  | Semi
  | Eof

val pp_token : Format.formatter -> token -> unit
val compare_token : token -> token -> int
val is_eof : token -> bool

type t

val for_string : string -> t
val for_channel : in_channel -> t
val pick : t -> token
val next : t -> token
val move : t -> unit
val drop : token -> t -> unit
