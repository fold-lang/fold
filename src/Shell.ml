
open Pure
open Fold
open Fold.Lex

module P = Parser.Make(OCaml)

let fold_parse_toplevel_phrase input eos_is_error =
  let lexer = Lexer.from_string input in
  let token = Lexer.read lexer in
  let state = Pratt.{ lexer; token } in

  match P.Statement.parse state with
  | Ok (syntax, state') ->
    UTop.Value (Parsetree.Ptop_def [syntax])
  | Error e -> UTop.Error ([], Pratt.error_to_string e)



let init () =
  if List.exists ((=) "camlp4o") !Topfind.predicates ||
     List.exists ((=) "camlp4r") !Topfind.predicates then
    print_endline "Fold is incompatible with camlp4!"
  else begin
    (* let use_file x = *)
    (*   List.map Reason_toolchain.To_current.copy_toplevel_phrase *)
    (*     (Reason_toolchain.JS.canonical_use_file x) *)
    (* in *)

    (* UTop.set_phrase_terminator ";"; *)
    UTop.prompt := fst (React.S.create LTerm_text.(eval [B_fg (LTerm_style.blue); S "-> "]));
    UTop.parse_toplevel_phrase := fold_parse_toplevel_phrase;

      (* UTop.parse_default ( *)

      (* Reason_util.correctly_catch_parse_errors *)
      (*   (fun x -> Reason_toolchain.To_current.copy_toplevel_phrase *)
      (*       (Reason_toolchain.JS.canonical_toplevel_phrase x)) *)
    (* ); *)

    (* UTop.parse_use_file := UTop.parse_default ( *)
    (*   Reason_util.correctly_catch_parse_errors use_file *)
    (* ); *)

    UTop.history_file_name :=
      Some (Filename.concat LTerm_resources.home ".fold-history");

    (* Toploop.parse_use_file := Reason_util.correctly_catch_parse_errors use_file; *)

    (* Printing in Reason syntax *)
    (* let open Reason_toolchain.From_current in *)
    (* let wrap f g fmt x = g fmt (f x) in *)
    (* Toploop.print_out_value := *)
    (*   wrap copy_out_value Reason_oprint.print_out_value; *)
    (* Toploop.print_out_type := *)
    (*   wrap copy_out_type Reason_oprint.print_out_type; *)
    (* Toploop.print_out_class_type := *)
    (*   wrap copy_out_class_type Reason_oprint.print_out_class_type; *)
    (* Toploop.print_out_module_type := *)
    (*   wrap copy_out_module_type Reason_oprint.print_out_module_type; *)
    (* Toploop.print_out_type_extension := *)
    (*   wrap copy_out_type_extension Reason_oprint.print_out_type_extension; *)
    (* Toploop.print_out_sig_item := *)
    (*   wrap copy_out_sig_item Reason_oprint.print_out_sig_item; *)
    (* Toploop.print_out_signature := *)
    (*   wrap (List.map copy_out_sig_item) Reason_oprint.print_out_signature; *)
    (* Toploop.print_out_phrase := *)
    (*   wrap copy_out_phrase Reason_oprint.print_out_phrase; *)
  end

let () =
  init ();
  UTop_main.main ()


