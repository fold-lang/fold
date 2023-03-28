module type Transform = sig
  type t

  val const : Parsetree.constant -> t
  val longident : Longident.t -> t
  val rec_module : t list -> t
  val module_ : t -> t
  val let_ : t list -> t
  val let_rec : t list -> t
  val val_ : t list -> t
  val val_rec : t list -> t
  val cases : t list -> t
  val arrow : t -> t -> t
  val constraint_ : t -> t -> t
  val block : t list -> t
  val tuple : t list -> t
  val apply : t -> t list -> t
  val label : optional:bool -> string -> t -> t
  val field : t -> Longident.t -> t
  val record : t list -> t option -> t
  val open_ : t -> t
  val binding : t -> t -> t
  val while_ : t -> t -> t
  val match_ : t -> t -> t
  val if_then : t -> t -> t
  val if_then_else : t -> t -> t -> t
  val case : t -> ?guard:t -> t -> t
  val for_ : ?down:bool -> t -> t -> t -> t
  val fn : t list -> t -> t
  val array : t list -> t
  val list : t list -> t option -> t
end
