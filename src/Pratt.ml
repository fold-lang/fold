
open Pure
open Syntax
open Lex

let () = Pervasives.print_endline "Pratt"

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
    if test (Token.literal state.token) then
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
      if Token.literal s.token = Symbol "EOF" then
        "%s unexpected end of file while reading %s" %
        (Location.show (Token.location s.token), label)
      else
      if label = (Literal.show (Symbol "EOF")) then
        "parsing stopped at %s" % Token.show s.token
      else
        "expected %s but %s found" % (label, Token.show s.token) in
    Error msg
  | Ok x -> Ok x

let expect token =
  exactly (Token.literal token) <?> Token.show token

let advance =
  modify begin fun s ->
    { s with token = Lexer.next s.lexer }
  end

let consume token =
  expect token >> lazy advance

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


(** [parse_rule rule] constructs a parser for a syntax described by [rule].

    {[
      (* Parse the 'x' symbol. *)
      parse_rule (symbol "x")

      (* Parse the 42 integer. *)
      parse_rule (int 42)

      (* Parse the plus operator. *)
      parse_rule (form [symbol "_"; symbol "+"; symbol "_"]))

      (* Parse the if-then-else expression. *)
      parse_rule (form (List.map symbol ["if"; "_"; "then"; "_"; "else"; "_"])))

      (* Parse a form. *)
      parse_rule (form [symbol "f"; symbol "x"; symbol "y"])))
    ]}

 *)
let parse_with_rule rule =
  let rec loop fqn acc atoms =
    match atoms with
    | [] ->
      return (Form (Expr.form (List.rev fqn) :: (List.rev acc)))

    | Atom (_loc, Symbol "_") as slot :: rest ->
      any >>= fun expr ->
      advance >> lazy (loop (slot :: fqn) (expr :: acc) rest)

    | Atom tok as keyword :: rest ->
      consume tok >> lazy (loop (keyword :: fqn) acc rest)

    | Form _ :: _ ->
      invalid_arg "invalid rule definition syntax" in

  match rule with
  | Form atoms ->
    loop [] [] atoms

  | Atom tok as atom ->
    consume tok >> lazy (return atom)


let rec parse_atom token =
  consume token >> lazy (return (Expr.atom token))

and parse_form left =
  prefix 90 >>= fun right ->
  let form_list =
    match left with
    | Form xs -> xs
    | atom    -> [atom] in
  return (Form (List.append form_list [right]))

and infix precedence left =
  get >>= fun { env; token } ->
  match Env.lookup token env with
  | Some rule -> parse_with_rule rule >>= fun right ->
    (* if rule.precedence > precedence *)
      (* then parse left >>= parse_infix precedence *)
      (* else return left *)
    fail "todo"
  | None -> parse_form

and prefix precedence =
  get >>= fun { env; token } ->
  let rule = Env.lookup token env or Atom token in
  parse_with_rule rule >>= fun left ->
  infix precedence left


let expression () =
  prefix 0

let parse lexer env =
  let state = { lexer; env; token = Lexer.read lexer } in
  match run (expression ()) state with
  | Ok (expr, _) -> Ok expr
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




