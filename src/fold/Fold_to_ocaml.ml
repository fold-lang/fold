open Prelude

module Ml_eval = struct
  type expr = Parsetree.expression
  type pat = Parsetree.pattern
  type vb = Parsetree.value_binding
  type opnd = Parsetree.open_declaration
  type structure_item = Parsetree.structure_item
  type signature_item = Parsetree.signature_item
  type mod' = Parsetree.module_expr
  type mb = Parsetree.module_binding
  type extension = Parsetree.extension

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

  open struct
    module Ml = struct
      include Ast_helper

      let unit_lid = Longident.Lident "()"
      let nil_lid = Longident.Lident "[]"
      let cons_lid = Longident.Lident "::"
    end

    let conv_const (const : Shaper.const) =
      match const with
      | Int x -> Ml.Const.int x
      | Float x -> Ml.Const.float (string_of_float x)
      | String x -> Ml.Const.string x
      | Char x -> Ml.Const.char x
  end

  module Expr = struct
    open struct
      let unit = Ml.Exp.construct (Location.mknoloc Ml.unit_lid) None
      let nil = Ml.Exp.construct (Location.mknoloc Ml.nil_lid) None

      let cons x xs =
        Ml.Exp.construct
          (Location.mknoloc Ml.cons_lid)
          (Some (Ml.Exp.tuple [ x; xs ]))
    end

    let ident _eval id = Ml.Exp.ident id
    let constant _eval c = Ml.Exp.constant (conv_const c)
    let field eval a id = Ml.Exp.field (eval.expr a) id
    let extension ext = Ml.Exp.extension ext

    let apply eval f_fl args_fl =
      let f_ml = eval.expr f_fl in
      let args_ml =
        List.map (fun arg_fl -> (Asttypes.Nolabel, eval.expr arg_fl)) args_fl
      in
      Ml.Exp.apply f_ml args_ml

    let construct eval id args_fl =
      match args_fl with
      | [] -> Ml.Exp.construct (Location.mknoloc id) None
      | [ arg_fl ] ->
        let arg_ml = eval.expr arg_fl in
        Ml.Exp.construct (Location.mknoloc id) (Some arg_ml)
      | _ ->
        (* [TODO] Add explicity arity ext like Reason. *)
        let arg_ml = Ml.Exp.tuple (List.map eval.expr args_fl) in
        Ml.Exp.construct (Location.mknoloc id) (Some arg_ml)

    let fn eval (args_fl : fl list) (body_fl : fl) =
      let body_ml = eval.expr body_fl in
      List.fold_left
        (fun acc arg_fl ->
          match (arg_fl : fl) with
          | Ident (Lower _id) ->
            let pat = eval.pat arg_fl in
            let label = Asttypes.Nolabel in
            let default = None in
            Ml.Exp.fun_ label default pat acc
          | _ ->
            Fmt.epr "@.args=%a@.body=%a@.@."
              (Fmt.Dump.list Shaper.dump)
              args_fl Shaper.dump body_fl;
            assert false
        )
        body_ml args_fl

    let case eval (fl : fl) =
      match fl with
      | Shape ("->", [ pat_fl; exp_fl ]) ->
        let pat_ml = eval.pat pat_fl in
        let exp_ml = eval.expr exp_fl in
        Ml.Exp.case ?guard:None pat_ml exp_ml
      | _ -> assert false

    let fn_match eval (cases_fl : fl list) =
      let cases_ml = List.map (case eval) cases_fl in
      Ml.Exp.function_ cases_ml

    let match' eval exp_fl cases_fl =
      let exp_ml = eval.expr exp_fl in
      let cases_ml = List.map (case eval) cases_fl in
      Ml.Exp.match_ exp_ml cases_ml

    let tuple eval items = Ml.Exp.tuple (List.map eval.expr items)

    let rec list eval ?spread:tl xs =
      match xs with
      | [] -> Option.fold ~some:eval.expr ~none:nil tl
      | x_fl :: xs_fl ->
        let x_ml = eval.expr x_fl in
        let xs_ml = list eval ?spread:tl xs_fl in
        cons x_ml xs_ml

    let let_in_unit eval vbl_fl =
      let vbl_ml = List.map eval.vb vbl_fl in
      Ml.Exp.let_ Asttypes.Nonrecursive vbl_ml unit

    let rec block eval (xs : fl list) =
      match xs with
      | [] -> unit
      | [ x ] -> eval.expr x
      | Shape ("let", [ Scope ("(", Seq (Some ",", vbl), ")") ]) :: xs
      | Shape ("let", [ Seq (Some ",", vbl) ]) :: xs ->
        let vbl_ml = List.map eval.vb vbl in
        let body_ml = block eval xs in
        Ml.Exp.let_ Asttypes.Nonrecursive vbl_ml body_ml
      | Shape ("let", [ vb ]) :: xs ->
        let vbl_ml = [ eval.vb vb ] in
        let body_ml = block eval xs in
        Ml.Exp.let_ Asttypes.Nonrecursive vbl_ml body_ml
      | Shape ("open", [ mexp ]) :: items ->
        let odcl = eval.opnd mexp in
        Ml.Exp.open_ odcl (block eval items)
      | item :: items -> Ml.Exp.sequence (eval.expr item) (block eval items)

    let if_then_else eval cond_fl then_fl else_fl =
      let cond_ml = eval.expr cond_fl in
      let then_ml = eval.expr then_fl in
      let else_ml = eval.expr else_fl in
      Ml.Exp.ifthenelse cond_ml then_ml (Some else_ml)

    let if_then eval cond_fl then_fl =
      let cond_ml = eval.expr cond_fl in
      let then_ml = eval.expr then_fl in
      Ml.Exp.ifthenelse cond_ml then_ml None

    let while' eval cond body =
      let cond_ml = eval.expr cond in
      let body_ml = eval.expr body in
      Ml.Exp.while_ cond_ml body_ml

    and if_cases eval (cs0 : fl list) =
      let rec loop (cs : fl list) =
        match cs with
        | [] -> None
        | Shape ("->", [ Ident (Lower "else"); else_body ]) :: _cs ->
          let else_body = eval.expr else_body in
          Some else_body
        | Shape ("->", [ cond; body ]) :: cs ->
          let cond = eval.expr cond in
          let body = eval.expr body in
          let else_body = loop cs in
          Some (Ml.Exp.ifthenelse cond body else_body)
        | _ -> failwith "invalid case syntax in if"
      in
      match cs0 with
      | [] -> failwith "empty cases in if"
      | Shape ("->", [ Ident (Lower "else"); _body ]) :: _cs ->
        failwith "no condition cases apart from else in if"
      | Shape ("->", [ cond; body ]) :: cs ->
        let cond = eval.expr cond in
        let body = eval.expr body in
        let else_body = loop cs in
        Ml.Exp.ifthenelse cond body else_body
      | _ -> failwith "invalid if syntax"
  end

  module Pat = struct
    open struct
      let _unit = Ml.Pat.construct (Location.mknoloc Ml.unit_lid) None
      let nil = Ml.Pat.construct (Location.mknoloc Ml.nil_lid) None

      let cons x xs =
        (* [TODO] tvars *)
        let tvars = [] in
        Ml.Pat.construct
          (Location.mknoloc Ml.cons_lid)
          (Some (tvars, Ml.Pat.tuple [ x; xs ]))
    end

    let var _eval id = Ml.Pat.var id
    let constant _eval c = Ml.Pat.constant (conv_const c)

    let construct eval id args_fl =
      match args_fl with
      | [] -> Ml.Pat.construct id None
      | [ arg_fl ] ->
        let arg_ml = eval.pat arg_fl in
        (* TODO *)
        let tvars = [] in
        Ml.Pat.construct id (Some (tvars, arg_ml))
      | _ ->
        (* [TODO] Add explicity arity ext like Reason. *)
        let arg_ml = Ml.Pat.tuple (List.map eval.pat args_fl) in
        let tvars = [] in
        Ml.Pat.construct id (Some (tvars, arg_ml))

    let rec list eval ?spread:tl xs =
      match xs with
      | [] -> Option.fold ~some:eval.pat ~none:nil tl
      | x_fl :: xs_fl ->
        let x_ml = eval.pat x_fl in
        let xs_ml = list eval ?spread:tl xs_fl in
        cons x_ml xs_ml
  end

  module Vb = struct
    let mk eval (pat_fl : fl) (exp_fl : fl) =
      let pat_ml = eval.pat pat_fl in
      let exp_ml = eval.expr exp_fl in
      Ml.Vb.mk pat_ml exp_ml
  end

  module Mb = struct
    let mk eval name mexp_fl =
      let mexp_ml = eval.mod' mexp_fl in
      Ml.Mb.mk name mexp_ml
  end

  module Opnd = struct
    let mk eval (fl : fl) =
      let popen_expr = eval.mod' fl in
      Parsetree.
        { popen_expr
        ; popen_override = Asttypes.Fresh
        ; popen_loc = Location.none
        ; popen_attributes = []
        }
  end

  module Mod = struct
    let structure eval items_fl =
      let items_ml = List.map eval.structure_item items_fl in
      Ml.Mod.structure items_ml

    let ident _eval id = Ml.Mod.ident id
  end

  module Structure_item = struct
    let value eval vbl_fl =
      let vbl_ml = List.map eval.vb vbl_fl in
      Ml.Str.value Asttypes.Nonrecursive vbl_ml

    let module' eval mb_fl =
      let mb_ml = eval.mb mb_fl in
      Ml.Str.module_ mb_ml

    let eval eval fl = Ml.Str.eval (eval.expr fl)
    let open' eval mexp = Ml.Str.open_ (eval.opnd mexp)
  end

  module Extension = struct
    let structure _eval id items =
      let payload = Parsetree.PStr items in
      (id, payload)

    let signature _eval id items =
      let payload = Parsetree.PSig items in
      (id, payload)
  end
end

include Fold_eval.Eval (Ml_eval)
