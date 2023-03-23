type format = Fl | Ml | Bin | Sexp

let run in_fmt out_fmt out_chan =
  match (in_fmt, out_fmt) with
  | Fl, Bin ->
    let fl = Fold_syntax.Parser.parse out_chan in
    let ml = Fold_syntax.To_ocaml.structure fl in
    Marshal.to_channel stdout ml []
  | Fl, Ml ->
    let fl = Fold_syntax.Parser.parse out_chan in
    let ml = Fold_syntax.To_ocaml.structure fl in
    let out = Format.formatter_of_out_channel stdout in
    Format.fprintf out "%a@." Fold_syntax.To_ocaml.Pp.structure ml
  | Fl, Sexp ->
    let fl = Fold_syntax.Parser.parse out_chan in
    let out = Format.formatter_of_out_channel stdout in
    Format.fprintf out "%a@." Fold_syntax.pp_sexp fl
  | _ -> invalid_arg "invalid conv"

let main in_fmt out_fmt in_file =
  let out_chan = open_in in_file in
  run in_fmt out_fmt out_chan
(* let structure = parse_structure chan in *)
(* let syn = Fold_syntax.Syntax_encoder.structure structure in *)
(* Fmt.pr "%a@." Fold_syntax.Syntax.pp syn *)
(* Formatter.print syn *)

open Cmdliner

let in_fmt_arg =
  let doc = "Read the input as one of the following formats: ml, fl or bin." in
  let opts =
    Arg.enum [ ("ml", Ml); ("fl", Fl); ("bin", Bin); ("sexp", Sexp) ]
  in
  Arg.(info [ "i"; "input" ] ~doc |> opt (some opts) None |> required)

let out_fmt_arg =
  let doc =
    "Write the output in one of the following formats: ml, fl or bin."
  in
  let opts =
    Arg.enum [ ("ml", Ml); ("fl", Fl); ("bin", Bin); ("sexp", Sexp) ]
  in
  Arg.(info [ "o"; "output" ] ~doc |> opt (some opts) None |> required)

let in_file_arg =
  let doc = "Input file path." in
  Arg.(info [] ~doc |> pos 0 (some file) (Some "/dev/stdin") |> required)

let () =
  Printexc.record_backtrace true;
  let doc = "Fold formatter" in
  let info = Cmdliner.Cmd.info "fold-fmt" ~version:"0.0.2" ~doc in
  let cmd =
    Cmdliner.Cmd.v info
      Cmdliner.Term.(const main $ in_fmt_arg $ out_fmt_arg $ in_file_arg)
  in
  cmd |> Cmdliner.Cmd.eval |> Stdlib.exit
