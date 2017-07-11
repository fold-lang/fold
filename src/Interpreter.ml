open Pure
open Lex


module type Self = sig

  module Name : sig
    type t

    val id : string -> t

    val dot : t -> string -> t

    val apply : t -> t -> t
  end


  module Type : sig
    type t

    (* A *)
    val constructor : Name.t -> t

    (* a *)
    val var : string -> t

    (* (t1, t2, ... , tn) *)
    val tuple : t list -> t
  end


  module Pattern : sig
    type t

    val token : Token.t -> t

    (* - C
       - M.C
       - M.C x
       - M.C (x, y)
     *)
    val constructor : Name.t -> t option -> t

    (* (t1, t2, ... , tn) *)
    val tuple : t list -> t
  end


  module Expression : sig
    type t

    val token : Token.t -> t

    (* let name = value in body *)
    val let' : Pattern.t -> t -> t -> t

    (* f x y *)
    val apply : t -> t list -> t

    (* (t1, t2, ... , tn) *)
    val tuple : t list -> t
  end

  (* Statement is a syntactic category for top-level phrases.
   *
   * Value, type, module definitions and import derictives are examples of
   * top-level statements. *)
  module Statement : sig
    type t

    (* `val (pattern <- pattern) `= (expression <- expression) *)
    val val' : Pattern.t -> Expression.t -> t

    (* `def (name <- Identifier.lowercase) (params <- pattern* )
     *   `= (expression <- Expression.t) *)
    val def : Pattern.t -> Expression.t -> t

    (* `type (name <- Identifier.capitalized) (parameters <- Identifier.lowercase* )
     *   `= (type <- Type.t) *)
    val type' : Name.t -> Name.t list -> Type.t -> t
  end
end
