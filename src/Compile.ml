

let compile statement =
  Compmisc.init_path false;
  Env.set_unit_name "Hello";
  undefined ()
  (* Fmt.pr "%a@.\n" Printast.structure *)

