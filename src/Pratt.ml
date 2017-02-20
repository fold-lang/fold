
open Pure
open Syntax
open Lex

module M = Map.Make(String)
module R = Result.Of_error(String)


module Precedence = struct
  (* ASSIGNMENT  = 1; *)
  (* CONDITIONAL = 2; *)
  (* SUM         = 3; *)
  (* PRODUCT     = 4; *)
  (* EXPONENT    = 5; *)
  (* PREFIX      = 6; *)
  (* POSTFIX     = 7; *)
  (* CALL        = 8; *)


  let terminal_precedence = 90

  let lookup name =
    match name with
    (* Match atomic symbols. *)
    | "<EOF>" -> Some 0
    | ";" -> Some 20
    (* Match symbols that can start an operator. *)
    | str ->
      begin match str.[0] with
      | '=' -> Some 10
      | '#' -> Some 20
      | '+' | '-' -> Some 30
      | '*' | '/' -> Some 40
      | '(' | '{' | '[' -> Some 80
      | _ -> None
    end
end


module rec State : sig
  type t = {
    lexer   : Lex.Lexer.t;
    token   : Lex.Token.t;
    env : Env.t;
  }
end = struct
  type t = {
    lexer   : Lex.Lexer.t;
    token   : Lex.Token.t;
    env : Env.t;
  }
end

and Env : sig
  type t

  val empty : t

  val lookup_prefix     : string -> t -> Parser.prefix option
  val lookup_infix      : string -> t -> Parser.infix  option
  val lookup_precedence : string -> t -> int           option

  val lookup_infix_rule : string -> t -> expr option

  val define_prefix     : string -> Parser.prefix -> t -> t
  val define_infix      : string -> Parser.infix  -> t -> t
  val define_precedence : string -> int           -> t -> t
end = struct
  type t = {
    data       : expr M.t;
    next       : t option;
    precedence : int M.t;
    prefix     : Parser.prefix M.t;
    infix      : Parser.infix M.t;
  }

  let empty = {
    data       = M.empty;
    next       = None;
    precedence = M.empty;
    prefix     = M.empty;
    infix      = M.empty;
  }

  let find k m =
    Option.catch (fun () -> M.find k m)

  let lookup_prefix     key {prefix}     = find key prefix
  let lookup_infix      key {infix}      = find key infix
  let lookup_precedence key {precedence} = find key precedence


  let rec lookup_infix_rule name self =
    let continue () =
      match self.next with
      | Some next -> lookup_infix_rule name next
      | None      -> None in

    if M.mem key self.data then
      let expr = M.find name self.data in
      match expr with
      | Form (Atom (_, Symbol "syntax"); Atom (_, Int prec)) ->
        Some x
      | _ -> continue ()
    else continue ()


  let define_syntax_rule rule self =
    let name =
      (* find_first *)
      match rule with
      | Atom (_, String name) :: _ -> name
      | Atom (_, Symbol _)
      | [] -> fail "invalid syntax rule"
    { self with data = M.add name rule self.data }


  let define_prefix     key x self = { self with prefix     = M.add key x self.prefix     }
  let define_infix      key x self = { self with infix      = M.add key x self.infix      }
  let define_precedence key x self = { self with precedence = M.add key x self.precedence }
end

and Parser : sig
  include StateT
    with type 'a monad = 'a R.t
     and type state = State.t

  type prefix = expr t
  type infix  = expr -> expr t
end = struct
  include StateT(State)(R)

  type prefix = Syntax.expr t
  type infix  = Syntax.expr -> Syntax.expr t
end

open Parser

let empty state = Error "empty"

let return = Parser.pure

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
  get >>= fun State.{token} ->
  if test (Token.literal token) then
    return (Expr.atom token)
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
      if Token.literal State.(s.token) = Symbol "EOF" then
        "%s unexpected end of file while reading %s" %
        (Location.show (Token.location State.(s.token)), label)
      else
      if label = (Literal.show (Symbol "EOF")) then
        "parsing stopped at %s" % Token.to_string State.(s.token)
      else
        "expected %s but %s found" % (label, Token.to_string State.(s.token)) in
    Error msg
  | Ok x -> Ok x


let expect literal =
  get >>= fun State.{ token = actual_token } ->
  exactly literal <?> Token.to_string actual_token


let advance =
  modify begin fun s ->
    State.{ s with token = Lexer.next s.lexer }
  end


let consume literal =
  expect literal >> lazy advance


let inspect f =
  get >>= fun s ->
  f s; put s


let inspect_token =
  inspect begin fun State.{ token } ->
    print ("token: %s" % Lex.Token.show token)
  end


let invalid_infix _left =
  get >>= fun State.{ token } ->
  error ("%s cannot be used in infix position" % Token.to_string token)


let invalid_prefix =
  get >>= fun State.{ token } ->
  error ("%s cannot be used in prefix position" % Token.to_string token)


let rec parse_atom token =
  advance >> lazy (return (Expr.atom token))




let rec parse_form left =
  prefix 90 >>= fun right ->
  let form_list =
    match left with
    | Form xs -> xs
    | atom    -> [atom] in
  return (Form (List.append form_list [right]))


and infix precedence left =
  print ("infix: precedence = %d, left = %s" % (precedence, Expr.to_string left));
  get >>= fun s ->

  let name = Literal.to_string (Token.literal State.(s.token)) in
  let env = State.(s.env) in

  let parser =
    env
    |> Env.lookup_infix_rule name
    |> Option.map parser_for_rule
    |> Option.with_default parse_form in

  if (Precedence.lookup name or 90) > precedence
    then parser left >>= infix precedence
    else return left


and prefix precedence =
  print ("prefix: precedence = %d" % precedence);
  get >>= fun s ->
  let k = Literal.to_string (Token.literal State.(s.token)) in
  let g = State.(s.env) in

  let parser =
    g
    |> Env.lookup_prefix k
    |> Option.with_default (parse_atom State.(s.token)) in

  parser >>= fun left ->
  infix precedence left


(* rule: x "+" y *)
and parser_for_rule rule left =
  let rec loop fqn acc atoms =
    match atoms with
    | [] ->
      return (Form (Expr.form (List.rev fqn) :: (List.rev acc)))

    | Atom (_loc, Symbol _) :: rest ->
      expression () >>= fun expr ->
      advance >> lazy (loop fqn (expr :: acc) rest)

    | Atom (_loc, (String _ as lit)) as keyword :: rest ->
      consume lit >> lazy (loop (keyword :: fqn) acc rest)

    | _ ->
      invalid_arg "invalid rule definition syntax" in

  match rule with
  | Form atoms ->
    loop [] [left] atoms

  | Atom (_, lit) as atom ->
    consume lit >> lazy (return atom)


and expression () =
  prefix 0


let add_infix k prec g =
  let p =
    fun left ->
    get >>= fun State.{token} ->
    advance >> lazy (expression ()) >>= fun right ->
    return Expr.(form [atom token; left; right]) in
  g
  |> Env.define_infix k p
  |> Env.define_precedence k prec


let add_prefix k g =
  let p =
    get >>= fun State.{token} ->
    advance >> lazy (expression ()) >>= fun right ->
    return Expr.(form [atom token; right]) in
  g
  |> Env.define_prefix k p


let eof g =
  g
  |> Env.define_infix "EOF" invalid_infix
  |> Env.define_precedence "EOF" 0
  |> Env.define_prefix "EOF" invalid_prefix


let default_grammar =
  Env.empty
  |> eof
  |> add_infix "+" 30
  |> add_infix "-" 30
  |> add_prefix "-"


let parse lexer =
  let state = State.{
      env = default_grammar;
      lexer;
      token = Lexer.read lexer
    } in
  match run (expression ()) state with
  | Ok (expr, _) -> Ok expr
  | Error msg    -> Error msg


let parse_string parser str =
  let lexer = Lexer.from_string str in
  let state = State.{
      env = Env.empty;
      lexer;
      token = Lexer.read lexer
    } in
  match run parser state with
  | Ok (expr, _) -> Ok expr
  | Error e -> Error e


