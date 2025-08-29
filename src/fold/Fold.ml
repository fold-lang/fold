module Parser = Fold_parser
module To_ocaml = Fold_to_ocaml
module Of_ocaml = Fold_of_ocaml
module Fmt = Fold_fmt
module Fl : Fold_ast_sig.Cons with type t = Shaper.syntax = Fold_ast.Cons
module Ml = Prelude.Ml

type fl = Fl.t
type ml = Ml.structure

module Utils = struct
  let output_binary_structure ~input_file_name oc (structure : ml) =
    set_binary_mode_out oc true;
    output_string oc Prelude.Current_ast.Ast.Config.ast_impl_magic_number;
    output_value oc input_file_name;
    output_value oc structure

  let parse_structure input =
    let lexbuf = Lexing.from_channel input in
    let structure = Ppxlib_ast.Parse.implementation lexbuf in
    Prelude.Conv.copy_structure structure

  let dump_parsetree = Ppxlib.Pprintast.structure
  let pprint_structure = Ppxlib.Pprintast.structure
end

let fl_of_ml : ml -> fl = Fold_of_ocaml.structure
let ml_of_fl : fl -> ml = Fold_to_ocaml.structure
let fmt : out_channel -> fl -> unit = Fold_fmt.print
let pp = Shaper.pp
let pp_sexp = Shaper.pp_sexp
let defmacro = Fold_macros.defmacro
