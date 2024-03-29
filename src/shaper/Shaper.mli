(* t = name | atom | list | form *)
type ident = Upper of string | Lower of string
type const = Int of int | Char of char | String of string | Float of float

type syntax = private
  | Ident of ident
  | Const of const
  | Sym of string
  | Scope of string * syntax * string
  | Seq of syntax list
  | Shape of Astlib.Location.t * string * syntax list

type loc = Astlib.Location.t

val noloc : Astlib.Location.t

(** {2 Construct syntax} *)

val lower : string -> syntax
val upper : string -> syntax
val sym : string -> syntax
val int : int -> syntax
val char : char -> syntax
val string : string -> syntax
val float : float -> syntax
val parens : syntax -> syntax
val brackets : syntax -> syntax
val braces : syntax -> syntax
val seq : syntax list -> syntax
val comma : ?loc:Astlib.Location.t -> syntax list -> syntax
val semi : ?loc:Astlib.Location.t -> syntax list -> syntax
val shape : ?loc:Astlib.Location.t -> string -> syntax list -> syntax

(** {2 Syntax predicates} *)

val is_scope : syntax -> bool
val is_seq : syntax -> bool
val is_seq_juxt : syntax -> bool
val is_shape : syntax -> bool
val is_lower : syntax -> bool
val is_upper : syntax -> bool

(** {2 Print shapes} *)

val pp_ident : Format.formatter -> ident -> unit
val pp_const : Format.formatter -> const -> unit
val pp_sep : string -> Format.formatter -> unit -> unit
val dump : syntax Fmt.t
val pp_sexp : syntax Fmt.t
val pp : syntax Fmt.t
