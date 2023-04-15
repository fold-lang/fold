module Ml_eval = struct
  include Ppxlib.Parsetree
  include Ppxlib.Ast_builder.Default

  let pstr x = Parsetree.PStr x
  let psig x = Parsetree.PSig x
  let ptyp x = Parsetree.PTyp x
  let ppat p ~guard = Parsetree.PPat (p, guard)
  let pexp_with_attributes pexp_attributes e = { e with pexp_attributes }
end

include Fold_eval.Eval (Ml_eval)
