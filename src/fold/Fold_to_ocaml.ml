module Ml_eval = struct
  include Ppxlib.Parsetree
  include Ppxlib.Ast_builder.Default

  let type_declaration ~loc ?(attrs = []) ~name ~params ~cstrs ~kind ~private_
      ~manifest () =
    { (type_declaration ~loc ~name ~params ~cstrs ~kind ~private_ ~manifest) with
      ptype_attributes = attrs
    }

  let value_description ~loc ?(attrs = []) ~name ~type_ ~prim () =
    { (value_description ~loc ~name ~type_ ~prim) with pval_attributes = attrs }

  let pcstr_tuple ctl = Pcstr_tuple ctl
  let pcstr_record ldl = Pcstr_record ldl
  let pstr x = Parsetree.PStr x
  let psig x = Parsetree.PSig x
  let ptyp x = Parsetree.PTyp x
  let ppat p ~guard = Parsetree.PPat (p, guard)
  let ptype_abstract = Ptype_abstract
  let ptype_variant cdl = Ptype_variant cdl
  let ptype_record ldl = Ptype_record ldl
  let ptype_open = Ptype_open
  let pexp_with_attributes pexp_attributes e = { e with pexp_attributes }

  let pext_decl ls constructor_arguments core_type =
    Pext_decl (ls, constructor_arguments, core_type)

  let pext_rebind id = Pext_rebind id
end

include Fold_eval.Eval (Ml_eval)
