open Prelude

type ident = Longident.t
type const = Ml.constant

(** AST fragments as types *)

type 'r apply = fl -> fl list -> 'r
type 'r array = fl list -> 'r
type 'r arrow = fl -> fl -> 'r
type 'r binding = fl -> fl -> 'r
type 'r block = fl list -> 'r
type 'r case = fl -> ?guard:fl -> fl -> 'r
type 'r cases = fl list -> 'r
type 'r constraint' = fl -> fl -> 'r
type 'r construct = ident -> fl list -> 'r
type 'r field = fl -> ident -> 'r
type 'r fn = fl list -> fl -> 'r
type 'r fn_match = fl list -> 'r
type 'r for' = ?down:bool -> fl -> fl -> fl -> 'r
type 'r for_all = fl list -> fl -> 'r
type 'r if_then = fl -> fl -> 'r
type 'r if_then_else = fl -> fl -> fl -> 'r
type 'r label = optional:bool -> string -> fl -> 'r
type 'r let' = fl list -> 'r
type 'r let_rec = fl list -> 'r
type 'r list' = ?spread:fl -> fl list -> 'r
type 'r match' = fl -> fl -> 'r
type 'r module' = fl -> 'r
type 'r module_rec = fl list -> 'r
type 'r open' = fl -> 'r
type 'r record = ?spread:fl -> fl list -> 'r
type 'r tuple = fl list -> 'r
type 'r val' = fl list -> 'r
type 'r val_rec = fl list -> 'r
type 'r while' = fl -> fl -> 'r

module type Cons = sig
  type t

  val const : const -> t
  val string : string -> t
  val int : int -> t
  val float : float -> t
  val char : char -> t
  val longident : ident -> t
  val lower : string -> t
  val apply : fl apply
  val array : fl array
  val arrow : fl arrow
  val binding : fl binding
  val block : fl block
  val case : fl case
  val cases : fl cases
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
  val let_rec : fl let_rec
  val list : fl list'
  val match' : fl match'
  val module' : fl module'
  val module_rec : fl module_rec
  val open' : fl open'
  val record : fl record
  val tuple : fl tuple
  val val' : fl val'
  val val_rec : fl val_rec
  val while' : fl while'
end

module type Eval = sig
  type t

  val eval :
       const:(const -> 'r)
    -> ident:(ident -> 'r)
    -> apply:'r apply
    -> array:'r array
    -> arrow:'r arrow
    -> binding:'r binding
    -> block:'r block
    -> case:'r case
    -> cases:'r cases
    -> constraint':'r constraint'
    -> construct:fl construct
    -> field:'r field
    -> fn:'r fn
    -> fn_match:'r fn_match
    -> for':'r for'
    -> for_all:'r for_all
    -> if_then:'r if_then
    -> if_then_else:'r if_then_else
    -> label:'r label
    -> let':'r let'
    -> let_rec:'r let_rec
    -> list:'r list
    -> match':'r match'
    -> module':'r module'
    -> module_rec:'r module_rec
    -> open':'r open'
    -> record:'r record
    -> tuple:'r tuple
    -> val':'r val'
    -> val_rec:'r val_rec
    -> while':'r while'
    -> t
    -> 'r

  (* val partial_eval : *)
  (*      ?const:(const -> 'r) *)
  (*   -> ?ident:(ident -> 'r) *)
  (*   -> ?apply:'r apply *)
  (*   -> ?array:'r array *)
  (*   -> ?arrow:'r arrow *)
  (*   -> ?binding:'r binding *)
  (*   -> ?block:'r block *)
  (*   -> ?case:'r case *)
  (*   -> ?cases:'r cases *)
  (*   -> ?constraint':'r constraint' *)
  (*   -> ?construct:'r construct *)
  (*   -> ?field:'r field *)
  (*   -> ?fn:'r fn *)
  (* -> fn_match:'r fn_match *)
  (*   -> ?for':'r for' *)
  (*   -> ?for_all:'r for_all *)
  (*   -> ?if_then:'r if_then *)
  (*   -> ?if_then_else:'r if_then_else *)
  (*   -> ?label:'r label *)
  (*   -> ?let':'r let' *)
  (*   -> ?let_rec:'r let_rec *)
  (*   -> ?list:'r list *)
  (*   -> ?match':'r match' *)
  (*   -> ?module':'r module' *)
  (*   -> ?module_rec:'r module_rec *)
  (*   -> ?open':'r open' *)
  (*   -> ?record:'r record *)
  (*   -> ?tuple:'r tuple *)
  (*   -> ?val':'r val' *)
  (*   -> ?val_rec:'r val_rec *)
  (*   -> ?while':'r while' *)
  (*   -> 'r *)
  (*   -> t *)
  (*   -> 'r *)
end
