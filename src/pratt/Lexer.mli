type token =
  | Int of int
  | Lower of string
  | Upper of string
  | Sym of string
  | String of string
  | Lbrace
  | Rbrace
  | Lparen
  | Rparen
  | Lbracket
  | Rbracket
  | Comma
  | Semi
  | Eof

val pp_token : Format.formatter -> token -> unit
val compare_token : token -> token -> int
val equal_token : token -> token -> bool
val is_eof : token -> bool
val is_upper : token -> bool
val is_lower : token -> bool

type t

val line_number : t -> int
val for_string : string -> t
val for_channel : ?file_name:string -> in_channel -> t
val pick : t -> token
val next : t -> token
val move : t -> unit
val drop : token -> t -> unit
val loc : t -> Astlib.Location.t
