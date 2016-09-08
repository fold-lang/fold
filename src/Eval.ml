
open Pure
open Syntax
open Lex


let is_macro_call expr env = undefined ()

let rec macroexpand expr env =
  if is_macro_call expr env then
    match expr with
    | Form ((Atom (loc, name)) :: args) ->
      begin match Env.lookup name env with
        (* FIXME *)
        (* | Some (Func f) -> *)
          (* macroexpand env (f args) *)
        | _ -> expr
      end
    | _ -> expr
  else
    expr


let rec eval expr env =
  match macroexpand expr env with
  (* META *)
  (* TODO: add_meta (add_meta x attr1) attr2 = add_meta x [attr1, attr2] *)

  (* get_meta (add_meta x attr) == Some attr *)
  | Form [Atom (_, Symbol "get_meta"); Form [Atom (_, Symbol "add_meta"); x; attr]] ->
    (Form [Expr.symbol "Some"; attr], env)

  (* define x (add_meta x attr) == (); get_meta x == Some attr *)
  | Form [Atom (_, Symbol "define");
          Atom (_, name);
          Form [Atom (_, Symbol "add_meta"); x; attr]] ->
    let (value, env') = eval x env in
    let env'' = Env.define name (Form [Expr.symbol "add_meta"; value; attr]) env' in
    (Expr.symbol "()", env'')

  (* get_meta x == (Option.map get_meta (Env.lookup x env)) *)
  | Form [Atom (_, Symbol "get_meta"); Atom (_, lit)] ->
    begin match Env.lookup lit env with
      | Some (Form [Atom (_, Symbol "add_meta"); value; attr]) ->
        (Form [Expr.symbol "Some"; attr], env)
      | _ ->
        (Expr.symbol "None", env)
    end

  (* add_meta x attr == x *)
  | Form [Atom (_, Symbol "add_meta"); x; _attr] ->
    eval x env

  (* DEFINE *)

  | Form [Atom (_, Symbol "define"); Atom (_, name); x] ->
    let (value, env) = eval x env in
    let env' = Env.define name value env in
    (Expr.symbol "()", env')

  | Form ((Atom (loc, Symbol "define")) :: _) ->
    fail ("invalid define syntax at %s" % Location.to_string loc)

  (* INFIX *)

  (* infix op precedence *)
  | Form [Atom (_, Symbol "infix"); Atom _ as op; Atom (_, Int precedence)] ->
    let rule = Expr.(Form [name "a"; op; name "b"]) in
    (rule, env)

  | Form [Atom (_, Symbol "infix"); Atom (_, name)] ->
    (* With default precedence. *)
    fail "infix with default precedence not imlemented"

  | Form ((Atom (loc, Symbol "infix")) :: _) ->
    fail ("invalid infix syntax at %s" % Location.to_string loc)

  (* MACRO *)

  | Form [Atom (_, Symbol "macro"); Atom (_, name); body] ->
    undefined ()

  | Form ((Atom (loc, Symbol "macro")) :: _) ->
    fail ("invalid macro syntax at %s" % Location.to_string loc)

  | _ -> undefined ()

