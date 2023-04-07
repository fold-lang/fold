open Prelude

module type Eval = sig
  type exp
  type pat
  type vb
  type opnd
  type str
  type mod'
  type mb

  type eval =
    { exp : fl -> exp
    ; pat : fl -> pat
    ; vb : fl -> vb
    ; opnd : fl -> opnd
    ; str : fl -> str
    ; mod' : fl -> mod'
    ; mb : fl -> mb
    }

  module Exp : sig
    val ident : eval -> Longident.t Location.loc -> exp
    val constant : eval -> Shaper.const -> exp
    val fn : eval -> fl list -> fl -> exp
    val fn_match : eval -> fl list -> exp
    val match' : eval -> fl -> fl list -> exp
    val tuple : eval -> fl list -> exp
    val block : eval -> fl list -> exp
    val list : eval -> ?spread:fl -> fl list -> exp
    val let_in_unit : eval -> fl list -> exp
    val if_then_else : eval -> fl -> fl -> fl -> exp
    val if_then : eval -> fl -> fl -> exp
    val if_cases : eval -> fl list -> exp
    val apply : eval -> fl -> fl list -> exp
    val construct : eval -> Longident.t -> fl list -> exp
    val while' : eval -> fl -> fl -> exp
  end

  module Pat : sig
    val var : eval -> string Location.loc -> pat
    val constant : eval -> Shaper.const -> pat
    val construct : eval -> Longident.t Location.loc -> fl list -> pat
  end

  module Vb : sig
    val mk : eval -> fl -> fl -> vb
  end

  module Opnd : sig
    val mk : eval -> fl -> opnd
  end

  module Str : sig
    val value : eval -> fl list -> str
    val module' : eval -> fl -> str
    val eval : eval -> fl -> str
    val open' : eval -> fl -> str
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

let () = defmacro "unless" unless_macro

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
  val exp : fl -> E.exp
  val structure : fl -> E.str list
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

  let rec eval : E.eval = { exp; pat; vb; opnd; str; mod'; mb }

  and exp (fl : fl) =
    match fl with
    (* -- let_in_unit -- *)
    | Shape ("let", [ Seq (Some ",", vbl) ]) -> E.Exp.let_in_unit eval vbl
    | Shape ("let", [ Scope ("(", Seq (Some ",", vbl), ")") ]) ->
      E.Exp.let_in_unit eval vbl
    | Shape ("let", [ vb ]) -> E.Exp.let_in_unit eval [ vb ]
    (* --- ident --- *)
    | Ident (Lower x) -> E.Exp.ident eval (Location.mknoloc (Longident.Lident x))
    | Sym x -> E.Exp.ident eval (Location.mknoloc (Longident.Lident x))
    (* --- const --- *)
    | Const const -> E.Exp.constant eval const
    (* --- fn --- *)
    | Shape ("->", [ Seq (None, args); body ]) -> E.Exp.fn eval args body
    | Shape ("->", [ arg; body ]) -> E.Exp.fn eval [ arg ] body
    | Scope ("{", Shape ("->", [ arg; body ]), "}") ->
      E.Exp.fn eval [ arg ] body
    (* --- fn_match --- *)
    | Scope ("{", Seq (Some ",", cases), "}") -> E.Exp.fn_match eval cases
    (* --- if_then_else --- *)
    | Shape ("if", [ a; b; c ]) -> E.Exp.if_then_else eval a b c
    (* --- if_then --- *)
    | Shape ("if", [ a; b ]) -> E.Exp.if_then eval a b
    | Shape ("if", [ Scope ("{", Seq (Some ",", cases), "}") ]) ->
      E.Exp.if_cases eval cases
    | Shape ("if", [ Scope ("{", c, "}") ]) -> E.Exp.if_cases eval [ c ]
    (* -- while -- *)
    | Shape ("while", [ cond; body ]) -> E.Exp.while' eval cond body
    (* --- match exp cases --- *)
    | Shape ("match", [ exp_fl; Scope ("{", Seq (Some ",", cases_fl), "}") ]) ->
      E.Exp.match' eval exp_fl cases_fl
    (* --- construct --- *)
    | Ident (Upper id) -> E.Exp.construct eval (Longident.Lident id) []
    | Scope ("(", Seq (None, []), ")") ->
      E.Exp.construct eval (Longident.Lident "()") []
    | Scope ("{", Seq (None, []), "}") ->
      E.Exp.construct eval (Longident.Lident "()") []
    | Seq (None, Ident (Upper c) :: args) ->
      E.Exp.construct eval (Longident.Lident c) args
    (* --- apply or macro --- *)
    | Seq (None, f :: args) -> E.Exp.apply eval f args
    (* --- a; b; b --- *)
    | Seq (Some ";", xs) -> E.Exp.block eval xs
    (* Err: a, b *)
    | Seq (Some _sep, _) -> assert false
    (* --- () --- *)
    | Scope ("(", Seq (Some ",", []), ")") ->
      E.Exp.construct eval (Longident.Lident "()") []
    | Scope ("(", Seq (Some ",", items), ")") -> E.Exp.tuple eval items
    (* --- group --- *)
    | Scope ("(", fl, ")") -> exp fl
    | Scope ("{", x, "}") -> exp x
    (* --- List --- *)
    (* list: [a, b & tl] *)
    | Scope ("[", Shape ("&", [ Seq (Some ",", items); tl ]), "]") ->
      E.Exp.list eval ?spread:(Some tl) items
    (* list: [a & tl] *)
    | Scope ("[", Shape ("&", [ a; tl ]), "]") ->
      E.Exp.list eval ?spread:(Some tl) [ a ]
    (* list: [a, b, c] --- *)
    | Scope ("[", Seq (Some ",", items), "]") ->
      E.Exp.list eval ?spread:None items
    (* list: [] *)
    | Scope ("[", Seq (None, []), "]") -> E.Exp.list eval ?spread:None []
    (* list: [a] *)
    | Scope ("[", item, "]") -> E.Exp.list eval ?spread:None [ item ]
    (* --- quote --- *)
    | Shape ("quote", [ x ]) -> exp (quasiquote x)
    | Shape ("quote", xs) -> exp (quasiquote (Shaper.seq xs))
    (* --- macro --- *)
    | Shape (kwd, args) -> begin
      match macroexpand kwd args with
      | Some expanded -> exp expanded
      | None -> Fmt.failwith "unknown macro %S" kwd
    end
    (* Err: unknown *)
    | _ ->
      Fmt.epr "--- unknown:@.%a@.---@." Shaper.dump fl;
      assert false

  and macroexpand kwd args =
    match getmacro kwd with
    | Some macro -> Some (macro args)
    | None -> None

  and pat (fl : fl) =
    match fl with
    | Ident (Lower id) -> E.Pat.var eval (Location.mknoloc id)
    (* Unit *)
    | Scope ("(", Seq ((None | Some ","), []), ")") ->
      E.Pat.construct eval (Location.mknoloc (Longident.Lident "()")) []
    | Const const -> E.Pat.constant eval const
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

  and str (fl : fl) =
    match fl with
    | Shape ("=", [ _lhs; _rhs ]) -> E.Str.value eval [ fl ]
    | Seq (Some ",", vbl) -> E.Str.value eval vbl
    (* | Shape ("let", [ Seq (Some ",", vbl) ]) -> value vbl *)
    (* | Shape ("let", [ vb ]) -> value [ vb ] *)
    | Shape ("module", [ mb ]) -> E.Str.module' eval mb
    | Shape ("open", [ mexp ]) -> E.Str.open' eval mexp
    | Ident _ | Const _ | Sym _ | Seq (None, _ :: _) | Scope _ | Seq (None, _)
      -> E.Str.eval eval fl
    | Shape (_kwd, _xs) ->
      Fmt.epr "Eval.str: %a@." Shaper.dump fl;
      assert false
    | Seq (Some _sep, _) -> assert false
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
    | Seq (Some ";", items) -> List.map str items
    | _ -> [ str fl ]
end
