open Ppxlib

let expander ~loc ~path:_ payload =
  match payload with
  (* Should have a single structure item, which is evaluation of a constant string. *)
  | PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ] ->
    let fl = Fold.To_ocaml.Embed.decode expr in
    (* Replace with a constant string with the value from the environment. *)
    let x = Shaper.lower "fl_todo" in
    Fold.To_ocaml.expression ~loc x
    (* Ast_builder.Default.estring ~loc (getenv sym) *)
  | _ -> Location.raise_errorf ~loc "[%%fl.unless] invalid syntax"

let extension =
  Context_free.Rule.extension
    (Extension.declare "fl.macro.expression" Expression
       Ast_pattern.(__)
       expander
    )

let () =
  Ppxlib.Driver.register_transformation ~rules:[ extension ] "ppx_fold_unless"
