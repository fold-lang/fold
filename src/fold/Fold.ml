module Parser = Parser
module To_ocaml = To_ocaml
module Ast = Shaper

type ast = Shaper.shape

let fmt : out_channel -> ast -> unit = Formatter.print
let pp = Ast.pp
let pp_sexp = Ast.pp_sexp

module Utils = struct
  let output_binary_structure ~input_file_name oc
      (structure : Parsetree.structure) =
    set_binary_mode_out oc true;
    output_string oc Config.ast_impl_magic_number;
    output_value oc input_file_name;
    output_value oc structure
end
