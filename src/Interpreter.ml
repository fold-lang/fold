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
    (* p as name *)

    type t

    val token : Token.t -> t
    (*
     * x
     * 'x'
     * "abc"
     * True
     * 42
     * 3.14
     *)

    val constructor : Name.t -> t option -> t
    (*
     * C
     * M.C
     * C x
     * C (x, y)
     * x & xs
     *)

    val tuple : t list -> t
    (* (t1, t2, ... , tn) *)
  end


  module Expression : sig
    type t

    val token : Token.t -> t

    (* let n1 = v1, n2 = v2, ... , nN = vN in body *)
    val let' : (Pattern.t * t) list -> t -> t

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

    (*
     * `val (pattern <- pattern) `= (expression <- expression)
     *
     *)
    val val' : Pattern.t -> Expression.t -> t

    (* `def (name <- Identifier.lowercase) (params <- pattern* )
     *   `= (expression <- Expression.t) *)
    val def : Pattern.t -> Expression.t -> t

    (* `type (name <- Identifier.capitalized) (parameters <- Identifier.lowercase* )
     *   `= (type <- Type.t) *)
    val type' : Name.t -> Name.t list -> Type.t -> t
  end

  module Module : sig
    type t
  end
end
