open Ppxlib

let getenv s = try Sys.getenv s with Not_found -> ""

let expander ~loc ~path:_ = function
  (* Should have a single structure item, which is evaluation of a constant string. *)
  | PStr
      [ { pstr_desc =
            Pstr_eval
              ( { pexp_loc = loc
                ; pexp_desc = Pexp_constant (Pconst_string (sym, _, None))
                ; _
                }
              , _
              )
        ; _
        }
      ] ->
    (* Replace with a constant string with the value from the environment. *)
    Ast_builder.Default.estring ~loc (getenv sym)
  | _ -> Location.raise_errorf ~loc "[%%fl.unless] invalid syntax"

let extension =
  Context_free.Rule.extension
    (Extension.declare "fl.unless" Expression Ast_pattern.(__) expander)

let () =
  Ppxlib.Driver.register_transformation ~rules:[ extension ] "ppx_fold_unless"
