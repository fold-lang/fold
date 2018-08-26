
module Lexer = Fold.Lex.Lexer
module Stream = Pratt.Stream
module Parser = Fold.Parser
module Syntax = Fold.Syntax


let compile_structure typed_structure =
  let bytecode =
    typed_structure
    |> Translmod.transl_toplevel_definition
    |> Simplif.simplify_lambda "hello.ml"
    |> Bytegen.compile_implementation "Hello"
  in
  let objfile = "_build/Hello" ^ ".cmo" in
  let oc = open_out_bin objfile in
  try
    Emitcode.to_file oc "Hello" objfile
      ~required_globals:Ident.Set.empty
      bytecode;
    Warnings.check_fatal ();
    close_out oc;
    Stypes.dump (Some ("Hello" ^ ".annot"))
  with x ->
    close_out oc;
    raise x


let () =
  Fmt.(set_style_renderer stdout `Ansi_tty);
  let rec loop input =
    if Stream.is_empty input then
      Fmt.pr "Main: done@."
    else
    begin
      match Parser.run' Parser.Structure.parser input with
      | Ok (structure_item, input') ->
        Fmt.pr "%a@.@." Pprintast.structure [structure_item];
        loop input'
      | Error Parser.P.Zero -> Fmt.pr "Main: empty result@."
      | Error e ->
        Fmt.pr "%s@." (Parser.P.error_to_string e)
    end
  in
  let stream = Lexer.(to_stream (of_channel stdin)) in
  loop stream

