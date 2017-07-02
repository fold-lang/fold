open Pure
open Lex


module type Self = sig
  type t

  module ID : sig
    (* Abc *)
    type capitalized

    (* abc *)
    type lowercase
  end

  module Type : sig
    type t

    (* A *)
    val constructor : ID.capitalized -> t

    (* a *)
    val var : ID.lowercase -> t

    (* (t1, t2, ... , tn) *)
    val tuple : t list -> t
  end


  module Pattern : sig
    type t

    val token : Token.t -> t
  end


  module Expression : sig
    type t

    val token : Token.t -> t

    (* let name = value in body *)
    val let' : Pattern.t -> t -> t -> t

    (* f x y *)
    val apply : t -> t list -> t
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
    val def : ID.lowercase -> Pattern.t -> Expression.t -> t

    (* `type (name <- Identifier.capitalized) (parameters <- Identifier.lowercase* )
     *   `= (type <- Type.t) *)
    val type' : ID.capitalized -> ID.lowercase list -> Type.t -> t
  end
end
