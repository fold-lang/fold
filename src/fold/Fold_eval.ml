open Prelude

module type Eval = sig
  type expr
  type pat
  type vb
  type opnd
  type structure_item
  type signature_item
  type mod'
  type mb
  type extension

  type eval =
    { expr : fl -> expr
    ; pat : fl -> pat
    ; vb : fl -> vb
    ; opnd : fl -> opnd
    ; structure_item : fl -> structure_item
    ; signature_item : fl -> signature_item
    ; mod' : fl -> mod'
    ; mb : fl -> mb
    ; extension : fl -> extension
    }

  module Expr : sig
    val ident : eval -> Longident.t Location.loc -> expr
    val extension : extension -> expr
    val constant : eval -> Shaper.const -> expr
    val field : eval -> fl -> Longident.t Location.loc -> expr
    val fn : eval -> fl list -> fl -> expr
    val fn_match : eval -> fl list -> expr
    val match' : eval -> fl -> fl list -> expr
    val tuple : eval -> fl list -> expr
    val block : eval -> fl list -> expr
    val list : eval -> ?spread:fl -> fl list -> expr
    val let_in_unit : eval -> fl list -> expr
    val if_then_else : eval -> fl -> fl -> fl -> expr
    val if_then : eval -> fl -> fl -> expr
    val if_cases : eval -> fl list -> expr
    val apply : eval -> fl -> fl list -> expr
    val construct : eval -> Longident.t -> fl list -> expr
    val while' : eval -> fl -> fl -> expr
  end

  module Extension : sig
    val structure :
      eval -> string Location.loc -> structure_item list -> extension

    val signature :
      eval -> string Location.loc -> signature_item list -> extension
  end

  module Pat : sig
    val var : eval -> string Location.loc -> pat
    val constant : eval -> Shaper.const -> pat
    val construct : eval -> Longident.t Location.loc -> fl list -> pat
    val list : eval -> ?spread:fl -> fl list -> pat
  end

  module Vb : sig
    val mk : eval -> fl -> fl -> vb
  end

  module Opnd : sig
    val mk : eval -> fl -> opnd
  end

  module Structure_item : sig
    val value : eval -> fl list -> structure_item
    val module' : eval -> fl -> structure_item
    val eval : eval -> fl -> structure_item
    val open' : eval -> fl -> structure_item
  end

  module Mb : sig
    val mk : eval -> string option Location.loc -> fl -> mb
  end

  module Mod : sig
    val structure : eval -> fl list -> mod'
    val ident : eval -> Longident.t Location.loc -> mod'
  end
end

type macro = Shaper.syntax list -> Shaper.syntax

module Env = Map.Make (String)

let env : macro Env.t ref = ref Env.empty
let defmacro tok p = env := Env.add tok p !env
let getmacro tok = Env.find_opt tok !env

(* --- unless --- *)

let unless_macro args =
  let module S = Shaper in
  match args with
  | [ cond; body ] ->
    S.shape "if" [ S.seq [ S.lower "not"; cond ]; body ]
    (* C.if_then (C.apply (C.lower "not") [ cond ]) body *)
  | _ -> failwith "rewrite: invalid unless args"

(* let () = defmacro "unless" unless_macro *)

(* foo *)

(*
    (define-syntax foo
      (lambda (stx)
        (syntax "I am foo")))
    ---
    syntax foo = fn _syn -> quote "I am foo";
  *)
let foo_macro _args = Shaper.string "I am foo"
let () = defmacro "foo" foo_macro

module Eval (E : Eval) : sig
  val expr : fl -> E.expr
  val structure : fl -> E.structure_item list
end = struct
  let rec quasiquote (syn : Shaper.syntax) =
    let open Fold_ast.Cons in
    let construct s = Fold_ast.Cons.construct (Longident.Lident s) in
    match syn with
    | Scope ("(", x, ")") -> quasiquote x
    | Ident (Upper id) -> construct "Ident" [ construct "Upper" [ string id ] ]
    | Ident (Lower id) -> construct "Ident" [ construct "Lower" [ string id ] ]
    | Const (Int x) -> construct "Const" [ construct "Int" [ int x ] ]
    | Const (Float x) -> construct "Const" [ construct "Float" [ float x ] ]
    | Const (Char x) -> construct "Const" [ construct "Char" [ char x ] ]
    | Const (String x) -> construct "Const" [ construct "String" [ string x ] ]
    | Sym x -> construct "Sym" [ string x ]
    | Seq (None, items) ->
      construct "Seq" [ construct "None" []; list (List.map quasiquote items) ]
    | Seq (Some sep, items) ->
      construct "Seq"
        [ construct "Some" [ string sep ]; list (List.map quasiquote items) ]
    | Scope (l, x, r) -> construct "Scope" [ string l; quasiquote x; string r ]
    | Shape ("unquote", [ code ]) -> code
    | Shape (kwd, items) ->
      construct "Shape" [ string kwd; list (List.map quasiquote items) ]

  let rec eval : E.eval =
    { expr; pat; vb; opnd; structure_item; signature_item; mod'; mb; extension }

  and list : 'a. (E.eval -> ?spread:fl -> fl list -> 'a) -> fl -> 'a =
   fun mk scoped ->
    match scoped with
    (* list: [a, b & tl] *)
    | Shape ("&", [ Seq (Some ",", items); tl ]) ->
      mk eval ?spread:(Some tl) items
    (* list: [a & tl] *)
    | Shape ("&", [ a; tl ]) -> mk eval ?spread:(Some tl) [ a ]
    (* list: [a, b, c] --- *)
    | Seq (Some ",", items) -> mk eval ?spread:None items
    (* list: [] *)
    | Seq (None, []) -> mk eval ?spread:None []
    (* list: [a] *)
    | item -> mk eval ?spread:None [ item ]

  and expr (fl : fl) =
    match fl with
    (* --- ident --- *)
    | Ident (Lower x) ->
      E.Expr.ident eval (Location.mknoloc (Longident.Lident x))
    | Sym x -> E.Expr.ident eval (Location.mknoloc (Longident.Lident x))
    (* M.x *)
    | Shape (".", [ Ident (Upper m); Ident (Lower v) ]) ->
      E.Expr.ident eval (Location.mknoloc (Longident.Ldot (Lident m, v)))
    (* a.b *)
    | Shape (".", [ a; Ident (Lower id) ]) ->
      E.Expr.field eval a (Location.mknoloc (Longident.Lident id))
    (* -- let_in_unit -- *)
    | Shape ("let", [ Seq (Some ",", vbl) ]) -> E.Expr.let_in_unit eval vbl
    | Shape ("let", [ Scope ("(", Seq (Some ",", vbl), ")") ]) ->
      E.Expr.let_in_unit eval vbl
    | Shape ("let", [ vb ]) -> E.Expr.let_in_unit eval [ vb ]
    (* --- const --- *)
    | Const const -> E.Expr.constant eval const
    (* --- fn --- *)
    | Shape ("->", [ Seq (None, args); body ]) -> E.Expr.fn eval args body
    | Shape ("->", [ arg; body ]) -> E.Expr.fn eval [ arg ] body
    | Scope ("{", Shape ("->", [ arg; body ]), "}") ->
      E.Expr.fn eval [ arg ] body
    (* --- fn_match --- *)
    | Scope ("{", Seq (Some ",", cases), "}") -> E.Expr.fn_match eval cases
    (* --- if_then_else --- *)
    | Shape ("if", [ a; b; c ]) -> E.Expr.if_then_else eval a b c
    (* --- if_then --- *)
    | Shape ("if", [ a; b ]) -> E.Expr.if_then eval a b
    | Shape ("if", [ Scope ("{", Seq (Some ",", cases), "}") ]) ->
      E.Expr.if_cases eval cases
    | Shape ("if", [ Scope ("{", c, "}") ]) -> E.Expr.if_cases eval [ c ]
    (* -- while -- *)
    | Shape ("while", [ cond; body ]) -> E.Expr.while' eval cond body
    (* --- match exp cases --- *)
    | Shape ("match", [ exp_fl; Scope ("{", Seq (Some ",", cases_fl), "}") ]) ->
      E.Expr.match' eval exp_fl cases_fl
    (* --- construct --- *)
    | Ident (Upper id) -> E.Expr.construct eval (Longident.Lident id) []
    | Scope ("(", Seq (None, []), ")") ->
      E.Expr.construct eval (Longident.Lident "()") []
    | Scope ("{", Seq (None, []), "}") ->
      E.Expr.construct eval (Longident.Lident "()") []
    | Seq (None, Ident (Upper c) :: args) ->
      E.Expr.construct eval (Longident.Lident c) args
    (* --- apply or macro --- *)
    | Seq (None, f :: args) -> E.Expr.apply eval f args
    (* --- a; b; b --- *)
    | Seq (Some ";", xs) -> E.Expr.block eval xs
    (* Err: a, b *)
    | Seq (Some _sep, _) -> assert false
    (* --- () --- *)
    | Scope ("(", Seq (Some ",", []), ")") ->
      E.Expr.construct eval (Longident.Lident "()") []
    | Scope ("(", Seq (Some ",", items), ")") -> E.Expr.tuple eval items
    (* --- group --- *)
    | Scope ("(", fl, ")") -> expr fl
    | Scope ("{", x, "}") -> expr x
    (* --- [...] --- *)
    | Scope ("[", scoped, "]") -> list E.Expr.list scoped
    (* --- quote --- *)
    | Shape ("quote", [ x ]) -> expr (quasiquote x)
    | Shape ("quote", xs) -> expr (quasiquote (Shaper.seq xs))
    (* --- macro --- *)
    | Shape (kwd, args) -> begin
      match getmacro kwd with
      | Some macro -> expr (macro args)
      | None ->
        (* Fmt.epr "--- unknown:@.%a@.---@." Shaper.dump fl;
           Fmt.failwith "unknown macro %S" kwd *)
        let id = Location.mknoloc "fl.macro_call" in
        let call = quasiquote fl in
        let item = E.Structure_item.eval eval call in
        let ext = E.Extension.structure eval id [ item ] in
        E.Expr.extension ext
    end
    (* Err: unknown *)
    | _ ->
      Fmt.epr "--- unknown:@.%a@.---@." Shaper.dump fl;
      assert false

  and pat (fl : fl) =
    match fl with
    | Ident (Lower id) -> E.Pat.var eval (Location.mknoloc id)
    (* Unit *)
    | Scope ("(", Seq ((None | Some ","), []), ")") ->
      E.Pat.construct eval (Location.mknoloc (Longident.Lident "()")) []
    | Const const -> E.Pat.constant eval const
    (* --- [...] --- *)
    | Scope ("[", scoped, "]") -> list E.Pat.list scoped
    | Sym _ -> assert false
    | _ ->
      Fmt.epr "todo pat: %a@." Shaper.dump fl;
      assert false

  and vb (fl : fl) =
    match fl with
    | Scope ("(", fl, ")") -> vb fl
    (* `a = b` *)
    | Shape ("=", [ pat_fl; exp_fl ]) -> E.Vb.mk eval pat_fl exp_fl
    | _ ->
      Fmt.epr "not a vb: %a@." Shaper.dump fl;
      assert false

  and structure_item (fl : fl) =
    match fl with
    | Shape ("=", [ _lhs; _rhs ]) -> E.Structure_item.value eval [ fl ]
    | Seq (Some ",", vbl) -> E.Structure_item.value eval vbl
    (* | Shape ("let", [ Seq (Some ",", vbl) ]) -> value vbl *)
    (* | Shape ("let", [ vb ]) -> value [ vb ] *)
    | Shape ("module", [ mb ]) -> E.Structure_item.module' eval mb
    | Shape ("open", [ mexp ]) -> E.Structure_item.open' eval mexp
    | Shape ("do", [ exp ]) ->
      let unit = Shaper.parens (Shaper.seq []) in
      let vb = Shaper.shape "=" [ unit; exp ] in
      E.Structure_item.value eval [ vb ]
    | Ident _ | Const _ | Sym _ | Seq (None, _ :: _) | Scope _ | Seq (None, _)
      -> E.Structure_item.eval eval fl
    | Shape (_kwd, _xs) ->
      Fmt.epr "Eval.str: %a@." Shaper.dump fl;
      assert false
    | Seq (Some _sep, _) -> assert false

  and extension (fl : fl) =
    match fl with
    | _ -> assert false

  and signature_item (fl : fl) =
    match fl with
    | _ -> assert false

  and mod' (fl : fl) =
    match fl with
    | Ident (Upper id) ->
      E.Mod.ident eval (Location.mknoloc (Longident.Lident id))
    | Scope ("{", Seq (None, []), "}") -> E.Mod.structure eval []
    | Scope ("{", Seq (Some ";", items_fl), "}") ->
      E.Mod.structure eval items_fl
    | _ ->
      Fmt.epr "todo: Mod:@.%a@." Shaper.dump fl;
      assert false

  and mb (fl : fl) =
    match fl with
    (* `a = b` *)
    | Shape ("=", [ Ident (Upper m_name); mexp_fl ]) ->
      let m_name = if String.equal m_name "_" then None else Some m_name in
      let m_name = Location.mknoloc m_name in
      E.Mb.mk eval m_name mexp_fl
    | _ ->
      Fmt.epr "not a mb: %a@." Shaper.dump fl;
      assert false

  and opnd (fl : fl) = E.Opnd.mk eval fl

  let structure (fl : fl) =
    match fl with
    | Seq (Some ";", items) -> List.map structure_item items
    | _ -> [ structure_item fl ]
end
