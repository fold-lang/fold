open Prelude

(** AST fragments as types *)

type 'r alt = fl list -> 'r
type 'r apply = fl -> fl list -> 'r
type 'r array = fl list -> 'r
type 'r arrow = fl -> fl -> 'r
type 'r binding = fl -> fl -> 'r
type 'r block = fl list -> 'r
type 'r sequence = fl list -> 'r
type 'r case = fl -> ?guard:fl -> fl -> 'r
type 'r cases = fl list -> 'r
type 'r constraint' = fl -> fl -> 'r
type 'r construct = ident -> fl list -> 'r
type 'r field = fl -> ident -> 'r
type 'r fn = fl list -> fl -> 'r
type 'r fn_match = fl list -> 'r
type 'r for' = down:bool -> fl -> fl -> fl -> 'r
type 'r for_all = fl list -> fl -> 'r
type 'r if_then = fl -> fl -> 'r
type 'r if_then_else = fl -> fl -> fl -> 'r
type 'r label = optional:bool -> string -> fl -> 'r
type 'r let' = fl -> 'r
type 'r list' = ?spread:fl -> fl list -> 'r
type 'r match' = fl -> fl list -> 'r
type 'r module' = fl -> 'r
type 'r module_rec = fl list -> 'r
type 'r open' = fl -> 'r
type 'r record = ?spread:fl -> fl list -> 'r
type 'r tuple = fl list -> 'r
type 'r while' = fl -> fl -> 'r

module type Cons = sig
  type t

  val const : constant -> t
  val string : string -> t
  val int : int -> t
  val float : float -> t
  val char : char -> t
  val longident : ident -> t
  val lower : string -> t
  val alt : fl alt
  val apply : fl apply
  val array : fl array
  val arrow : fl arrow
  val binding : fl binding
  val block : fl block
  val case : fl case
  val constraint' : fl constraint'
  val construct : fl construct
  val field : fl field
  val fn : fl fn
  val fn_match : fl fn_match
  val for' : fl for'
  val for_all : fl for_all
  val if_then : fl if_then
  val if_then_else : fl if_then_else
  val label : fl label
  val let' : fl let'
  val let_rec : fl let'
  val list : fl list'
  val match' : fl match'
  val module' : fl module'
  val module_rec : fl module_rec
  val open' : fl open'
  val record : fl record
  val tuple : fl tuple
  val while' : fl while'
end
