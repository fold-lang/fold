
open Pure
open Syntax
open Lex

type state = {
  lexer : Lexer.t;
  token : Token.t;
  env   : Env.t;
}
module State = struct
  type t = state
end

module Parser =
  StateT (State) (Result.Of_error(String))

include Parser

let empty state = Error "empty"

let return = pure

let error msg = fun _ -> Error msg

let (<|>) p1 p2 =
  fun state ->
    match p1 state with
    | Error _  -> p2 state
    | Ok value -> Ok value

let between op ed x = op >> x << ed

let option x p = p <|> return x
let optional p = option () (p >> lazy (return ()))

let rec skip_many x = optional (x >>= fun _ -> skip_many x)

let rec many p =
  option [] (p >>= fun x  -> many p
               >>= fun xs -> return (x :: xs))

let satisfy test =
  get >>= fun state ->
    if test (Token.value state.token) then
      return (Expr.atom state.token)
    else
      error "could not satisfy test"

let any        = satisfy (const true)
let exactly x  = satisfy ((=) x)
let one_of  xs = satisfy (fun x -> List.mem x xs)
let none_of xs = satisfy (fun x -> not (List.mem x xs))
let range s e  = satisfy (fun x -> s <= x && x <= e)


let (<?>) parser label s =
  match parser s with
  | Error _ ->
    let msg =
      if Token.value s.token = Symbol "EOF" then
        "%s unexpected end of file while reading %s" %
        (Location.show (Token.location s.token), label)
      else
      if label = (Literal.show (Symbol "EOF")) then
        "parsing stopped at %s" % Token.show s.token
      else
        "expected %s but %s found" % (label, Token.show s.token) in
    Error msg
  | Ok x -> Ok x

let expect x =
  exactly x <?> Literal.show x

let advance =
  modify begin fun s ->
    { s with token = Lexer.next s.lexer }
  end

let consume x =
  expect x >> lazy advance

type 'a rule =
  | Prefix of (Expr.t Parser.t)
  | Infix  of (Expr.t -> 'a Parser.t) * int

let invalid_infix () =
  let parser exp =
    get >>= fun { token } ->
    error ("%s cannot be used in infix position" % Token.show token) in
  Infix (parser, 0)

let invalid_prefix () =
  let parser =
    get >>= fun { token } ->
    error ("%s cannot be used in prefix position" % Token.show token) in
  Prefix parser

(* let parse_prefix right_precedence = *)
  (* state <- get; *)
  (* let { rule; grammar } = state in *)
  (* let default_nud = Option.force (grammar.default rule.sym).nud in *)
  (* let current_nud = rule.nud or default_nud in *)
  (* left <- current_nud; *)
  (* parse_infix rbp left *)

let expression right_precedence =
  undefined ()

(* XXX: We have an opportunity to define rewrite rules for optimization here! *)

(* let parse_rule rule = *)
  (* let rec loop acc expr_list = *)
    (* match expr_list with *)
    (* | [] -> List.rev acc *)
    (* | Atom (_, (Name _)) :: tail -> *)
      (* expression 0 >>= fun expr -> *)
      (* loop (expr :: acc) tail *)
    (* | Atom (_, literal) -> *)
      (* consume literal >> *)
    (* | _ -> fail "invalid macro syntax" in *)
  (* match rule with *)
  (* | Form (head :: tail) -> *)
    (* Form (loop [] expr_list) *)
  (* | _ -> fail "invalid macro syntax" *)


(* List.fold *)
  (* (fun r expr -> *)
    (* match expr with *)
    (* | Atom (loc, Name name) -> *)
      (* r >> lazy (expression 0) *)
    (* | Atom (loc, literal) -> *)
      (* r >> consume literal) *)
  (* empty *)

(* let base_env = *)
  (* Env.empty *)
  (* |> Env.add (Symbol "`") () *)

let parse_rule rule =
  let rec loop rule_name acc atom =
    match atom with
    | [] ->
      let name = Option.force rule_name in
      print ~file:stderr ("macro: done for rule %s" % (Token.to_string name));
      return (Form (Expr.atom name :: (List.rev acc)))

    | Atom (loc, Name name) :: rest ->
      (* XXX: any is used temporary instead of expression. *)
      any >>= fun value ->
      print ~file:stderr ("macro: %s = %s" % (name, Expr.show value));
      advance >> lazy (loop rule_name (value :: acc) rest)

    | Atom (loc, Symbol keyword) :: rest ->
      print ~file:stderr ("macro: keyword %s" % keyword);
      let rule_name =
        if is_some rule_name then
          rule_name
        else
          Some (loc, Symbol keyword) in
      consume (Name keyword) >> lazy (loop rule_name acc rest)

    | Atom (loc, lit) :: _ ->
      invalid_arg ("invalid literal %s in macro definition" % Literal.to_string lit)

    | Form _ :: _ ->
      invalid_arg "rewrite rules are not supported" in

  match rule with
  | Form atoms ->
    loop None [] atoms

  | Atom (loc, lit) as atom ->
    consume lit >> lazy (return atom)

let parse_form left =
  expression 90 >>= fun right ->
  let form_list =
    match left with
    | Form xs -> xs
    | atom         -> [atom] in
  return (Form (List.append form_list [right]))

let infix precedence left =
  get >>= fun { env; token } ->
  let rule = Env.lookup (Token.value token) env or (Expr.symbol "...") in
  if
  parse_rule rule >>= fun right ->


let prefix precedence =
  get >>= fun { env; token } ->
  let rule = Env.lookup (Token.value token) env or Atom token in
  parse_rule rule >>= fun left ->
  infix precedence left

let expression () =
  prefix 0

let parse (lexer : Lexer.t) env =
  let state = { lexer; env; token = Lexer.read lexer } in
  match run (expression ()) state with
  | Ok (expr, _) -> expr
  | Error msg    -> Error msg

let parse_string parser str =
  let lexer = Lex.Lexer.from_string str in
  let state = {
    lexer;
    token = Lex.Lexer.read lexer;
    env   = Env.empty
  } in
  match run parser state with
  | Ok (expr, _) -> Ok expr
  | Error e -> Error e

