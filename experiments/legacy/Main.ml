
open Pure

open Fold
open Fold.Lex


module P = Parser.Make(OCaml)


let compile_structure typed_structure =
  let bytecode =
    typed_structure
    |> Translmod.transl_toplevel_definition
    |> Simplif.simplify_lambda
    |> Bytegen.compile_implementation "Hello"
  in
  let objfile = "Hello" ^ ".cmo" in
  let oc = open_out_bin objfile in
  try
    bytecode |> Emitcode.to_file oc "Hello" objfile;
    Warnings.check_fatal ();
    close_out oc;
    Stypes.dump (Some ("Hello" ^ ".annot"))
  with x ->
    close_out oc;
    raise x


let () =
  let rec loop state c =
    match P.Statement.parse state with
    | Ok (syntax, state') ->

      let structure = [syntax] in

      (* Print *)
      Fmt.pr "%a@" Printast.top_phrase (Parsetree.Ptop_def structure);

      Compmisc.init_path false;
      let env = Compmisc.initial_env () in

      Env.set_unit_name "Hello";

      let (structure', signature, env') = Typemod.type_toplevel_phrase env structure in
      compile_structure structure';

      loop state' (c + 1)


    | Error Pratt.Empty -> ()
    | Error e ->
      print (" * Error: " ^ Pratt.error_to_string e)
  in
  let lexer = Lexer.from_channel stdin in
  let token = Lexer.read lexer in
  loop Pratt.{ lexer; token } 0

