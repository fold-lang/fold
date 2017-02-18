
open Pure
open Syntax
open Lex

module M = Map.Make(String)
module R = Result.Of_error(String)


module rec State : sig
  type t = {
    lexer   : Lex.Lexer.t;
    token   : Lex.Token.t;
    grammar : Grammar.t;
  }
end = struct
  type t = {
    lexer   : Lex.Lexer.t;
    token   : Lex.Token.t;
    grammar : Grammar.t;
  }
end

and Grammar : sig
  type t

  val empty : t

  val lookup_prefix     : string -> t -> Parser.prefix option
  val lookup_infix      : string -> t -> Parser.infix  option
  val lookup_precedence : string -> t -> int           option

  val define_prefix     : string -> Parser.prefix -> t -> t
  val define_infix      : string -> Parser.infix  -> t -> t
  val define_precedence : string -> int           -> t -> t
end = struct
  type t = {
    precedence : int M.t;
    prefix     : Parser.prefix M.t;
    infix      : Parser.infix M.t;
  }

  let empty = {
    precedence = M.empty;
    prefix     = M.empty;
    infix      = M.empty;
  }

  let find k m =
    Option.catch (fun () -> M.find k m)

  let lookup_prefix     key {prefix}     = find key prefix
  let lookup_infix      key {infix}      = find key infix
  let lookup_precedence key {precedence} = find key precedence

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


let invalid_infix () =
  let parser exp =
    get >>= fun State.{ token } ->
    error ("%s cannot be used in infix position" % Token.show token) in
  (parser, 0)

let invalid_prefix () =
  let parser =
    get >>= fun State.{ token } ->
    error ("%s cannot be used in prefix position" % Token.show token) in
  parser


let rec parse_atom token =
  consume (Token.literal token) >> lazy (return (Expr.atom token))

let rec parse_form left =
  prefix 90 >>= fun right ->
  let form_list =
    match left with
    | Form xs -> xs
    | atom    -> [atom] in
  return (Form (List.append form_list [right]))


and infix precedence left =
  get >>= fun s ->
  let k = Literal.to_string (Token.literal State.(s.token)) in
  let g = State.(s.grammar) in

  let parser = Grammar.lookup_infix k g or parse_form in
  if (Grammar.lookup_precedence k g or 0) > precedence
    then parser left >>= infix precedence
    else return left


and prefix precedence =
  get >>= fun s ->
  let k = Literal.to_string (Token.literal State.(s.token)) in
  let g = State.(s.grammar) in

  let parser = Grammar.lookup_prefix k g or parse_atom State.(s.token) in

  parser >>= fun left ->
  infix precedence left


let expression () = prefix 0


let parse lexer =
  let state = State.{
      grammar = Grammar.empty;
      lexer;
      token = Lexer.read lexer
    } in
  match run (expression ()) state with
  | Ok (expr, _) -> Ok expr
  | Error msg    -> Error msg


let parse_string parser str =
  let lexer = Lexer.from_string str in
  let state = State.{
      grammar = Grammar.empty;
      lexer;
      token = Lexer.read lexer
    } in
  match run parser state with
  | Ok (expr, _) -> Ok expr
  | Error e -> Error e

