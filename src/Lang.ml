open Pure
open Base

open Lex


module type Syntax = sig
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

    (* let name = value in body *)
    val let' : Pattern.t -> t -> t -> t
  end

  (* Statement is a syntactic category for top-level phrases.
   *
   * Value, type, module definitions and import derictives are examples of
   * top-level statements. *)
  module Statement : sig
    type t

    (* `val (pattern <- pattern) `= (expression <- expression) *)
    val val' : Pattern.t -> Expression.t -> t

    (* `def (name <- Identifier.lowercase) (params <- pattern* ) `= (expression <- Expression.t) *)
    val def : ID.lowercase -> Pattern.t list -> Expression.t -> t

    (* `type (name <- Identifier.capitalized) (parameters <- Identifier.lowercase* ) `= (type <- Type.t) *)
    val type' : ID.capitalized -> ID.lowercase list -> Type.t -> t
  end
end


module AST = struct
  module ID = struct
    type capitalized = string [@@deriving show]
    type lowercase   = string [@@deriving show]
  end

  module Pattern = struct
    type t = [
      | token
    ] [@@deriving show]

    let token x = x
  end

  module Expression = struct
    type t = [
      | `Let of Pattern.t * t * t
      | `Apply of t * t list
      | `Lambda of Pattern.t list * t
      | token
    ] [@@deriving show]

    let let' pat expr body = `Let (pat, expr, body)
  end

  module Type = struct
    type t = [
      | `Constructor of ID.capitalized
      | `Var of ID.lowercase
      | `Tuple of t list
    ] [@@deriving show]

    let constructor name = `Constructor name
    let var name = `Var name
    let tuple types = `Tuple types
  end

  module Statement = struct
    type t = [
      | `Val of Pattern.t * Expression.t
      | `Def of ID.lowercase * Pattern.t list * Expression.t
      | `Type of ID.capitalized * ID.lowercase list * Type.t
    ] [@@deriving show]

    let val' pat expr : t = `Val (pat, expr)
    let def name params expr = `Def (name, params, expr)
    let type' name params t = `Type (name, params, t)
  end

  type t = [
    | Pattern.t
    | Expression.t
    | Type.t
    | Statement.t
  ] [@@deriving show]

  include Printable.Make(struct
      type nonrec t = t
      let pp = pp
    end)
end


module Parser = struct
  module Expression = struct
    open Pratt

    let grammar =
      Grammar.init ~atom:(function tok -> singleton (tok :> AST.Expression.t)) [
        infix 30  "+" (fun x y -> `Apply (`Symbol "+", [x; y]));
        infix 30  "-" (fun x y -> `Apply (`Symbol "-", [x; y]));
        prefix    "-" (fun x   -> `Apply (`Symbol "-", [x]))
      ]

    let parse () =
      nud 0 grammar
  end

  module Pattern = struct
    open Pratt

    let grammar =
      Grammar.init ~atom:(fun tok -> singleton (tok :> AST.Pattern.t)) []

    let parse () =
      satisfy (function `Symbol x -> true | _ -> false) >>= fun pat ->
      advance >>= fun () -> pure pat
  end

  module Statement = struct
    open Pratt

    let val' g =
      consume (`Symbol "val") >>= fun () ->
      Pattern.parse () >>= fun pattern ->
      consume (`Symbol "=") >>= fun () ->
      Expression.parse () >>= fun value ->
      pure (AST.Statement.val' pattern value)

    let grammar : AST.Statement.t grammar =
      Grammar.init [
        Prefix (`Symbol "val", val');
        delimiter "=";
      ]
  end

end




(* let juxtaposition token = *)
(*   let precedence = 90 in *)
(*   let parse x = *)
(*     Pratt.parse_prefix precedence >>= fun y -> *)
(*     let list = *)
(*       match x with *)
(*       | `Form xs -> List.append xs [y] *)
(*       | atom    -> [atom; y] in *)
(*     Parser.pure (`Form list) in *)
(*   (parse, precedence) *)


(* let default_operator token = *)
(*   token *)
(*   |> Precedence.lookup *)
(*   |> Option.map (fun precedence -> *)
(*       let parse x = *)
(*         Parser.advance >>= fun () -> *)
(*         parse_prefix precedence >>= fun y -> *)
(*         Parser.pure (`Form [(token :> Syntax.t); x; y]) in *)
(*       (parse, precedence)) *)


(* let define_syntax_operators grammar = *)
(*   grammar *)
(*   |> postfix 70 "*" (fun x   -> Syntax.form [`Symbol "*"; x]) *)
(*   |> postfix 70 "+" (fun x   -> Syntax.form [`Symbol "+"; x]) *)
(*   |> postfix 70 "?" (fun x   -> Syntax.form [`Symbol "?"; x]) *)
(*   |> infix   30 "|" (fun x y -> Syntax.form [`Symbol "|"; x; y]) *)


(* let define_syntax = *)
(*   let parse = *)
(*     Parser.modify begin fun state -> *)
(*       State.{ state with grammar = *)
(*                 define_syntax_operators (Grammar.new_scope state.grammar) } *)
(*     end >>= fun () -> *)
(*     Parser.advance >>= fun () -> *)
(*     expression >>= fun x -> *)
(*     Parser.modify begin fun state -> *)
(*       State.{ state with grammar = Grammar.pop_scope state.grammar } *)
(*     end >>= fun () -> Parser.pure (`Form [`Symbol "syntax"; x]) in *)
(*   Grammar.define_prefix (`Symbol "syntax") parse *)

(* let grammar = *)
  (* Grammar.init *)
    (* ~atom:(fun x -> x :> AST.t) *)
    (* ~form:(fun x -> default_operator x or lazy (juxtaposition x)) *)
    (* () *)
  (* |> define_syntax *)

  (* |> val_syntax *)
  (* |> delimiter "=" *)

  (* |> between "(" ")" (fun x -> x) *)

