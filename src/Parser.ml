
module Loc = Location

module Ast = struct
	module Structure = Ast_helper.Str
	module Expression = Ast_helper.Exp
	module Pattern = struct
		include Ast_helper.Pat

		let var str = var (Location.mknoloc str)
	end
	module Constant = Ast_helper.Const

	let binding = Ast_helper.Vb.mk
end

open Local
open Lex

module S = Syntax

module P = Pratt.Make(Token)
let (>>=)  = P.(>>=)
let (<|>)  = P.(<|>)
let return = P.return

let (<@>) f mx = P.(mx >>= fun x -> return (f x))

let map f p =
  p >>= fun x -> return (f x)

let run' p input = P.run p input

let run p input =
  match P.run p input with
  | Ok (x, input') when Pratt.Stream.is_empty input' -> Ok x
  | Ok (_x, input') ->
		let input' = Pratt.Stream.to_list input' in
		Fmt.epr "** Leftover: %a@." (Fmt.Dump.list Token.pp) input';
		raise (Failure "XXX: Input letfover: %")
  | Error e -> Error e

let symbol s = P.consume (`Symbol s)

let sep1 ~by:sep p =
  p >>= fun x ->
  P.many (sep >>= fun () ->
    P.current >>= fun t ->
    Fmt.epr "?? %a@." Token.pp t;
    p) >>= fun xs ->
  P.return (x, xs)


(* TODO: Consider adding an Id token type.
 * See: https://caml.inria.fr/pub/docs/manual-ocaml/lex.html#capitalized-ident *)
let capitalized_id : string P.parser =
  P.current >>= fun token ->
  match token with
  | `Symbol name when Char.Ascii.is_upper (String.get name 0) ->
      P.advance >>= fun () ->
      return name
  | _ -> P.(error (unexpected_token token))


let module_name = capitalized_id

let constructor_name = capitalized_id

(*
 constant ::= integer-literal
            ∣ float-literal
            ∣ char-literal
            ∣ string-literal
            ∣ false
            ∣ true
 *)
let constant =
  P.current >>= fun t ->
  match t with
  | (`Int    _ as const)
  | (`Float  _ as const)
  | (`Char   _ as const)
  | (`String _ as const)
  | (`Bool   _ as const) ->
    P.advance >>= fun () -> P.return (S.Constant.make const)
  | _ -> P.(error (unexpected_token t))

let constant' : Parsetree.constant P.parser =
  P.current >>= fun token ->
	let return x =
		P.advance >>= fun () -> P.return x in
	match token with
	| `Char   x -> return (Ast.Constant.char x)
	| `String x -> return (Ast.Constant.string x)
	| `Int    x -> return (Ast.Constant.int x)
	| `Float  x -> return (Ast.Constant.float (string_of_float x)) (* XXX *)
	| _ -> P.(error (unexpected_token token))


let lowercase_id : string P.parser =
  P.current >>= fun token ->
  match token with
  | `Symbol name when Char.Ascii.is_lower (String.get name 0) ->
      P.advance >>= fun () ->
      return name
  | _ -> P.(error (unexpected_token token))

let value_name = lowercase_id


(* module_path = module_name ("." module_name)* *)
let module_path =
  sep1 ~by:(symbol ".") module_name >>= fun (x, xs) ->
  return (x, xs)


(* value_path = (module_path ".")? value_name *)
let value_path =
  let module_path_dot =
    module_path >>= fun (m0, ms) ->
    P.consume (`Symbol ".") >>= fun () ->
    P.return (m0 :: ms) in
  P.default [] module_path_dot >>= fun path ->
  value_name >>= fun vname ->
  P.return (Syntax.Name.lower ~path vname)


let constructor_path =
  let module_path_dot =
    module_path >>= fun (m0, ms) ->
    P.consume (`Symbol ".") >>= fun () ->
    P.return (m0 :: ms) in
  P.default [] module_path_dot >>= fun path ->
  constructor_name >>= fun cname ->
  P.return (Syntax.Name.upper ~path cname)


module rec Expr : sig
  val parser : S.Expr.t P.parser
  val grammar : S.Expr.t P.grammar
  val term : 'a P.grammar -> S.Expr.t P.parser
  val constant : Parsetree.expression P.parser
end = struct
  let parse ?precedence:(rbp = 0) (g : S.Expr.t P.grammar) : S.Expr.t P.parser =
    let left =
      P.some (P.nud g) >>= fun (x, _xs) -> (* XXX: xs is not used? *)
      let xs = [] in
      if List.length xs = 0 then
        P.return x
      else
        P.return (S.Expr.app x xs) in
    left >>= P.led rbp g


  (* let parse_let_binding g = *)
  (*   Pattern.parser >>= fun p -> *)
  (*   symbol "=" >>= fun () -> *)
  (*   parse g >>= fun e -> *)
  (*   P.return (p, e) *)


  (* let parse_let g : S.Expr.t P.parser = *)
  (*   symbol "let" >>= fun () -> *)
  (*   sep1 ~by:(symbol ",") (parse_let_binding g) >>= fun (lb, lbs) -> *)
  (*   symbol "in" >>= fun () -> *)
  (*   parse g >>= fun body -> *)
  (*   P.return (S.Expr.let' (lb :: lbs) body) *)


  (*
   constructor ::= ()
                 ∣ constr
                 ∣ begin end
                 ∣ []
                 ∣ [||]
                 ∣ `tag-name
   *)
  (* constructor *)
  (* | `Symbol "()" *)
  (* | `Symbol "[]" *)
  (* TODO: Empty array *)
  (* TODO: Polymorphic variant *)


  let term _g =
    map Syntax.Expr.constant constant
    <|> map Syntax.Expr.name value_path
    <|> map Syntax.Expr.constructor constructor_path


  let parse_group_or_tuple g =
    symbol "(" >>= fun () ->
    sep1 ~by:(symbol ",") (parse g) >>= fun (x, xs) ->
    symbol ")" >>= fun () ->
    if List.length xs = 0 then
      return x
    else
      return (S.Expr.tuple (x :: xs))


  let infix_left precedence tok f =
    let p g a =
      P.advance >>= fun () ->
      parse ~precedence g >>= fun b ->
      P.return (f a b) in
    P.left precedence tok p


  (* let parse_case g = *)
  (*   P.optional (P.consume (`Symbol "|")) >>= fun () -> *)
  (*   Pattern.parser >>= fun p -> *)
  (*   P.consume (`Symbol "->") >>= fun () -> *)
  (*   parse g >>= fun v -> *)
  (*   P.return (S.Expr.case p v) *)


  (* let _parse_fun_some g = *)
  (*   P.consume (`Symbol "fun") >>= fun () -> *)
  (*   P.some (parse_case g) >>= fun (c, cs) -> *)
  (*   P.return (S.Expr.fun' (c :: cs)) *)


  let _with_debug p = fun input ->
    Fmt.pl "PRE: %a" (Fmt.Dump.list Token.pp) (Pratt.Stream.to_list input);
    match p input with
    | Ok (v, input') ->
      Fmt.pl "POS: %a" (Fmt.Dump.list Token.pp) (Pratt.Stream.to_list input');
      Ok (v, input')

    | Error e ->
      Fmt.pl ">>> with_debug: error={%a}" (Fmt.of_to_string P.error_to_string) e;
      Error e


  let rec _many_with_error p =
    let open P in
    default []
      (p >>= fun x -> _many_with_error p >>= fun xs -> return (x :: xs))


  (* let parse_fun_many g = *)
  (*   P.consume (`Symbol "fun") >>= fun () -> *)
  (*   parse_case g >>= fun c -> *)
  (*   (many_with_error (with_debug (parse_case g))) >>= fun cs -> *)
  (*   P.return (S.Expr.fun' (c :: cs)) *)


  (* let _parse_fun_direct g = *)
  (*   P.consume (`Symbol "fun") >>= fun () -> *)
  (*   parse_case g >>= fun c1 -> *)
  (*   parse_case g >>= fun c2 -> *)
  (*   P.return (S.Expr.fun' [c1; c2]) *)


  (* let parse_fun = parse_fun_many *)


  let parse_if g =
    P.consume (`Symbol "if") >>= fun () ->
    parse g >>= fun condition ->
    P.consume (`Symbol "then") >>= fun () ->
    parse g >>= fun consequence ->
    P.consume (`Symbol "else") >>= fun () ->
    parse g >>= fun alternative ->
    P.return (S.Expr.if' condition consequence alternative)


  (* Parses Long identifiers and record access *)
  (* X.Y.Z.x: ((X . Y) . Z) . x *)
  (* X.Y.Z.{ ... } *)
  (* let parse_dot g left = *)
  (*   P.advance >>= fun () -> *)
  (*   match left with *)
  (*   (* New module path *) *)
  (*   | `Symbol m when Char.Ascii.is_upper (String.get m 0) -> *)
  (*     parse ~precedence:89 g >>= fun right -> *)
  (*     begin match right with *)
  (*      | `Symbol x -> *)
  (*        P.return S.(Expr.name (Name.(dot (id m) x))) *)
  (*      (* TODO: Check for other tokens: M.3 should be invalid *) *)
  (*      | `App (`Symbol f, args) -> *)
  (*        let m_name = S.Name.id m in *)
  (*        let full_f = S.Expr.name (S.Name.dot m_name f) in *)
  (*        P.return S.(Expr.app full_f args) *)
  (*      | other -> failwith "Symbol: Pexp_open non implemented" *)
  (*     end *)

  (*   (* Existing module path *) *)
  (*   | `Name n -> *)
  (*     parse ~precedence:89 g >>= fun right -> *)
  (*     begin match right with *)
  (*      | `Symbol x -> *)
  (*        P.return S.(Expr.name (Name.(dot n x))) *)
  (*      | other -> failwith "Name: Pexp_open not implemented" *)
  (*     end *)

    (* Record field *)
    (* | record_expr -> *)
    (*   parse ~precedence:89 g >>= fun right -> *)
    (*   begin match right with *)
    (*    | `Symbol x -> P.return S.(Expr.field record_expr (Name.id x)) *)
    (*    | other -> *)
    (*      failwith (Fmt.strf "Invalid record field syntax: %a" S.Expr.pp right) *)
    (*   end *)


  (* let parse_match g = *)
  (*   P.consume (`Symbol "match") >>= fun () -> *)
  (*   parse g >>= fun e -> *)
  (*   P.consume (`Symbol "with") >>= fun () -> *)

  (*   P.current >>= fun t -> *)
  (*   Fmt.pl ">>> parse_match: current = %a" Token.pp t; *)

  (*   P.some (parse_case g) >>= fun (c, cs) -> *)

  (*   P.current >>= fun t -> *)
  (*   Fmt.pl ">>> parse_match: current = %a" Token.pp t; *)

  (*   P.return (S.Expr.match' e (c :: cs)) *)

  let _binop _name _a _b =
    S.Expr.constant (S.Constant.make (`Int 42))

  let grammar =
    P.grammar [
      P.term term;

      (* P.null (`Symbol "let") parse_let; *)

      P.null (`Symbol "-")
        (fun g -> symbol "-" >>= fun () ->
          parse g >>= fun a -> P.return (S.Expr.(app (name (`Var "-")) [a])));

      infix_left 30 (`Symbol "+")
        (fun a b -> S.Expr.(app (name (`Var "+")) [a; b]));

      infix_left 40 (`Symbol "/")
        (fun a b -> S.Expr.(app (name (`Var "/")) [a; b]));

      infix_left 40 (`Symbol "*")
        (fun a b -> S.Expr.(app (name (`Var "*")) [a; b]));

      infix_left 40 (`Symbol "%")
        (fun a b -> S.Expr.(app (name (`Var "%")) [a; b]));

      infix_left 19 (`Symbol ";")
        (fun a b -> S.Expr.(app (name (`Var ";")) [a; b]));

      infix_left 50 (`Symbol "&")
        (fun a b -> S.Expr.(app (name (`Var "&")) [a; b]));

      infix_left 25 (`Symbol "|>")
        (fun a b -> S.Expr.(app (name (`Var "|>")) [a; b]));

      (* P.left 89 (`Symbol ".") parse_dot; *)

      (* P.null (`Symbol "fun") parse_fun; *)
      P.null (`Symbol "if") parse_if;

      (* P.null (`Symbol "match") parse_match; *)

      P.delimiter (`Symbol "val");
      P.delimiter (`Symbol "|");
      P.delimiter (`Symbol "->");
      P.delimiter (`Symbol "with");
      P.delimiter (`Symbol "def");
      P.delimiter (`Symbol "then");
      P.delimiter (`Symbol "else");
      P.delimiter (`Symbol "in");
      P.delimiter (`Symbol ",");

      P.null (`Symbol "(") parse_group_or_tuple;
      P.delimiter (`Symbol ")");
    ]

  let parser =
    parse grammar

	let constant =
		constant' >>= fun x ->
		P.return (Ast.Expression.constant x)
end

and Pattern : sig
	val var : Parsetree.pattern P.parser
	val constant : Parsetree.pattern P.parser
  val parser : Parsetree.pattern P.parser
end = struct
  (* let parse ?precedence:(rbp = 0) g = *)
  (*   let left = *)
  (*     P.some (P.nud g) >>= fun (x, xs) -> *)
  (*     if List.length xs = 0 then *)
  (*       P.return x *)
  (*     else *)
  (*       P.return (S.Pattern.app x xs) in *)
  (*   left >>= P.led rbp g *)

	let infix_left precedence tok f =
		let p g a =
			P.advance >>= fun () ->
			P.parse ~precedence g >>= fun b ->
			P.return (f a b) in
		P.left precedence tok p

  let _parse_term _g =
    let constant_p =
      constant >>= (P.return << S.Pattern.constant) in
    let value_name_p =
			value_name >>= (P.return << S.Pattern.var) in
    constant_p <|> value_name_p



	let constant = Ast.Pattern.constant <@> constant'

	let var = Ast.Pattern.var <@> lowercase_id

	let simple _g =
		constant <|> var

  let list_cons_operator =
    infix_left 50 (`Symbol "&")
      (fun a b ->
        let constr = Ast.Pattern.construct (Loc.mknoloc (Longident.Lident "::")) in
        let arg = Ast.Pattern.tuple [a; b] in
        constr (Some arg))

	let list g =
    P.consume (`Symbol "[") >>= fun () ->
    let g' =
      g |> P.Grammar.new_scope
        |> P.Grammar.add list_cons_operator in
    let empty_list = Ast.Pattern.construct (Loc.mknoloc (Longident.Lident "[]")) None in
    P.default empty_list (P.parse g') >>= fun p ->
    P.consume (`Symbol "]") >>= fun () ->
    return p


  let group_or_tuple g =
    P.consume (`Symbol "(") >>= fun () ->
    sep1 ~by:(P.consume (`Symbol ",")) (P.parse g) >>= fun (x, xs) ->
    symbol ")" >>= fun () ->
    if List.length xs = 0 then
      return x
    else
      return (Ast.Pattern.tuple (x :: xs))


  let grammar = P.grammar [
      P.term simple;

      (* infix_left 30 (`Symbol "+") *)
      (*   (fun a b -> S.Pattern.(app (var "+") [a; b])); *)

      (* infix_left 40 (`Symbol "/") *)
      (*   (fun a b -> S.Pattern.(app (var "/") [a; b])); *)

      (* infix_left 40 (`Symbol "*") *)
      (*   (fun a b -> S.Pattern.(app (var "*") [a; b])); *)

      (* infix_left 40 (`Symbol "%") *)
      (*   (fun a b -> S.Pattern.(app (var "%") [a; b])); *)

      P.null (`Symbol "[") list;

      P.null (`Symbol "(") group_or_tuple;
      P.delimiter (`Symbol ",");
      P.delimiter (`Symbol "]");
      P.delimiter (`Symbol ")");
      P.delimiter (`Symbol "=");
      (* P.delimiter (`Symbol "->"); *)
      (* P.delimiter (`Symbol "|"); *)
    ]

  let parser = P.parse grammar
end





(* FIXME: What about constructors? *)
(* TODO: Add operator_name *)
(* value-name  ::=  lowercase-ident   *)
(*    ∣   ( operator-name )   *)
let value_name =
  lowercase_id >>= fun x ->
  return x


let binding _g =
  Pattern.parser >>= fun p ->
	P.consume (`Symbol "=") >>= fun () ->
	Expr.constant >>= fun e ->
	P.return (Ast.binding p e)



module Structure = struct
  let parse_val g =
    P.consume (`Symbol "val") >>= fun () ->
		(* Optional rec flag. *)
		P.default Asttypes.Nonrecursive
			(P.consume (`Symbol "*") >>= fun () ->
			 P.return Asttypes.Recursive) >>= fun rec_flag ->
		binding g >>= fun b ->
    return (Ast.Structure.value rec_flag [b])

  let grammar = P.grammar [
      P.null (`Symbol "val") parse_val;
    ]

  let parser =
    P.parse grammar
end



