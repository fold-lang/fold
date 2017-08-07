
open Pure

open Fold
open Fold.Lex

module P = Parser.Make(OCaml)


let compile_structure typed_structure =
  let bytecode =
    typed_structure
    |> Translmod.transl_toplevel_definition
    |> Simplif.simplify_lambda (* "hello.ml" *)
    |> Bytegen.compile_implementation "Hello"
  in
  let objfile = "Hello" ^ ".cmo" in
  let oc = open_out_bin objfile in
  try
    Emitcode.to_file oc "Hello" objfile
      (* ~required_globals:Ident.Set.empty *)
      bytecode;
    Warnings.check_fatal ();
    close_out oc;
    Stypes.dump (Some ("Hello" ^ ".annot"))
  with x ->
    close_out oc;
    raise x


let () =
  let rec loop input =
    match Pratt.run P.Statement.parse input with
    | Ok syntax ->

      let structure = [syntax] in

      (* Print *)
      Fmt.pr "%a@" Printast.top_phrase (Parsetree.Ptop_def structure);

      Compmisc.init_path false;
      let env = Compmisc.initial_env () in

      Env.set_unit_name "Hello";

      let (structure', signature, env') = Typemod.type_toplevel_phrase env structure in
      compile_structure structure';

      loop input

    | Error Pratt.Empty -> print "Done"
    | Error e ->
      print (Pratt.error_to_string Token.pp e)
  in
  loop (Lexer.(from_channel stdin |> iter))

