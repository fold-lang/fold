
let () = Pervasives.print_endline "Eval"

open Pure
open Syntax
open Lex

let log = print ~file:stderr


let is_macro_call expr env =
  (* XXX *)
  false

let rec macroexpand expr env =
  if is_macro_call expr env then
    match expr with
    | Form ((Atom token) :: args) ->
      begin match Env.lookup token env with
        (* FIXME *)
        (* | Some (Func f) -> *)
          (* macroexpand env (f args) *)
        | _ -> expr
      end
    | _ -> expr
  else
    expr

exception NameError of string


let rec eval_expr expr env =
  match expr with
  | Form xs -> (Form (List.map (fun x -> fst (eval x env)) xs), env)
  | Atom (_, (Symbol x as key) as token) ->
    begin match Env.lookup token env with
      | Some value -> log ("looking up symbol %s" % x); (value, env)
      | None -> raise (NameError (Lex.Literal.to_string key))
    end
  | atomic_value -> (atomic_value, env)


and eval expr env =
  match macroexpand expr env with
  (* META *)
  (* TODO: add_meta (add_meta x attr1) attr2 = add_meta x [attr1, attr2] *)

  (* get_meta (add_meta x attr) == Some attr *)
  | Form [Atom (_, Symbol "get_meta"); Form [Atom (_, Symbol "add_meta"); x; attr]] ->
    (Form [Expr.symbol "Some"; attr], env)

  (* define x (add_meta x attr) == (); get_meta x == Some attr *)
  | Form [Atom (_, Symbol "define");
          Atom name;
          Form [Atom (_, Symbol "add_meta"); x; attr]] ->
    let (value, env') = eval x env in
    let env'' = Env.define name (Form [Expr.symbol "add_meta"; value; attr]) env' in
    (Expr.symbol "()", env'')

  (* get_meta x == (Option.map get_meta (Env.lookup x env)) *)
  | Form [Atom (_, Symbol "get_meta"); Atom x] ->
    begin match Env.lookup x env with
      | Some (Form [Atom (_, Symbol "add_meta"); value; attr]) ->
        (Form [Expr.symbol "Some"; attr], env)
      | _ ->
        (Expr.symbol "None", env)
    end

  (* add_meta x attr == x *)
  | Form [Atom (_, Symbol "add_meta"); x; _attr] ->
    eval x env

  (* add_meta x: invalid syntax *)
  | Form [Atom (_, Symbol "add_meta"); x] ->
    fail "invalid syntax, expecting attribute for `add_meta`"

  (* DEFINE *)

  | Form [Atom (_, Symbol "define"); Atom name; x] ->
    log ("eval: defining symbol %s" % Lex.Token.to_string name);
    let (value, env) = eval x env in
    let env' = Env.define name value env in
    (Expr.symbol "()", env')

  | Form ((Atom (loc, Symbol "define")) :: _) ->
    fail ("invalid define syntax at %s" % Location.to_string loc)

  (* INFIX *)

  (* infix op precedence *)
  | Form [Atom (_, Symbol "infix"); Atom _ as op; Atom (_, Int precedence)] ->
    let rule = Expr.(Form [symbol "_"; op; symbol "_"]) in
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

  | expr -> eval_expr expr env


