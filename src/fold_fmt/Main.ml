type format = Fl | Ml | Bin | Sexp | Parsetree

let run ~input_file_name ~input_fmt ~output_fmt ic oc =
  match (input_fmt, output_fmt) with
  | Fl, Bin ->
    let fl = Fold.Parser.parse_chan ic in
    let ml = Fold.To_ocaml.structure fl in
    Fold.Utils.output_binary_structure ~input_file_name oc ml
  | Fl, Ml ->
    let fl = Fold.Parser.parse_chan ic in
    let ml = Fold.To_ocaml.structure fl in
    let out = Format.formatter_of_out_channel oc in
    Format.fprintf out "%a@." Fold.Utils.pprint_structure ml
  | Ml, Fl ->
    let ml = Fold.Utils.parse_structure ic in
    let fl = Fold.fl_of_ml ml in
    Fold.fmt oc fl
  | Ml, Sexp ->
    let ml = Fold.Utils.parse_structure ic in
    let fl = Fold.fl_of_ml ml in
    let out = Format.formatter_of_out_channel oc in
    Format.fprintf out "%a@." Fold.pp_sexp fl
  | Fl, Sexp ->
    let fl = Fold.Parser.parse_chan ic in
    let out = Format.formatter_of_out_channel oc in
    Format.fprintf out "%a@." Fold.pp_sexp fl
  | Fl, Fl ->
    let fl = Fold.Parser.parse_chan ic in
    Fold.fmt oc fl
  | Ml, Ml ->
    let ml = Fold.Utils.parse_structure ic in
    let out = Format.formatter_of_out_channel oc in
    Format.fprintf out "%a@." Fold.Utils.pprint_structure ml
  | Ml, Bin ->
    let ml = Fold.Utils.parse_structure ic in
    Fold.Utils.output_binary_structure ~input_file_name oc ml
  | Fl, Parsetree ->
    let fl = Fold.Parser.parse_chan ic in
    let ml = Fold.To_ocaml.structure fl in
    let out = Format.formatter_of_out_channel oc in
    Format.fprintf out "%a@." Fold.Utils.dump_parsetree ml
  | _ -> failwith "unsupported conversion format"

let main input_fmt output_fmt input_file_name =
  let ic =
    if String.equal input_file_name "-" then stdin else open_in input_file_name
  in
  let oc = stdout in
  run ~input_file_name ~input_fmt ~output_fmt ic oc

open Cmdliner

let input_fmt_arg =
  let doc = "Read the input as one of the following formats: ml, fl or bin." in
  let opts =
    Arg.enum [ ("ml", Ml); ("fl", Fl); ("bin", Bin); ("sexp", Sexp) ]
  in
  Arg.(info [ "i"; "input" ] ~doc |> opt (some opts) None |> required)

let output_fmt_arg =
  let doc =
    "Write the output in one of the following formats: ml, fl or bin."
  in
  let opts =
    Arg.enum
      [ ("ml", Ml)
      ; ("fl", Fl)
      ; ("bin", Bin)
      ; ("sexp", Sexp)
      ; ("parsetree", Parsetree)
      ]
  in
  Arg.(info [ "o"; "output" ] ~doc |> opt (some opts) None |> required)

let input_file_name_arg =
  let doc = "Input file path." in
  Arg.(info [] ~doc |> pos 0 (some string) (Some "/dev/stdin") |> required)

let () =
  Printexc.record_backtrace true;
  let doc = "Fold formatter" in
  let info = Cmdliner.Cmd.info "fold-fmt" ~version:"0.0.2" ~doc in
  let cmd =
    Cmdliner.Cmd.v info
      Cmdliner.Term.(
        const main $ input_fmt_arg $ output_fmt_arg $ input_file_name_arg
      )
  in
  cmd |> Cmdliner.Cmd.eval |> Stdlib.exit
