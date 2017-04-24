open Pure
open Syntax
open Lex


module P = Parser


module M = struct
  include Map.Make(String)

  let find k m =
    Option.catch (fun () -> find k m)
end


module rec Grammar : sig
  type prefix = expr Parser.t
  type infix  = (expr -> expr Parser.t) * int

  type rule =
    | Infix  of infix
    | Prefix of prefix

  type t = {
    prefix : prefix M.t;
    infix  : infix  M.t;
    next   : t option;
  }

  val empty : t

  val define_prefix : string -> prefix -> t -> t
  val define_infix : string -> infix -> t -> t
  val define : string -> rule -> t -> t

  val lookup_prefix : string -> t -> prefix option
  val lookup_infix : string -> t -> infix option
end = struct
  type prefix = expr Parser.t
  type infix  = (expr -> expr Parser.t) * int

  type rule =
    | Infix  of infix
    | Prefix of prefix

  type t = {
    prefix : prefix M.t;
    infix  : infix  M.t;
    next   : t option;
  }


  let empty = {
    prefix = M.empty;
    infix  = M.empty;
    next   = None;
  }


  let define_infix name rule self =
    { self with infix = M.add name rule self.infix  }


  let define_prefix name rule self =
      { self with prefix = M.add name rule self.prefix }


  let define name rule =
    match rule with
    | Infix  infix  -> define_infix  name infix
    | Prefix prefix -> define_prefix name prefix


  let rec lookup_prefix name { prefix; next } =
    match M.find name prefix with
    | None -> Option.(next >>= lookup_prefix name)
    | some -> some


  let rec lookup_infix name { infix; next } =
    match M.find name infix with
    | None -> Option.(next >>= lookup_infix name)
    | some -> some
end


and State : sig
  type t = {
    lexer   : Lexer.t;
    grammar : Grammar.t;
    token   : Token.t option;
  }

  val grammar : t -> Grammar.t

  include P.Input with type t := t
                   and type item = Token.t

  val init : ?grammar: Grammar.t -> lexer: Lexer.t -> unit -> t
end = struct
  type t = {
    lexer   : Lexer.t;
    grammar : Grammar.t;
    token   : Token.t option;
  }

  let grammar self = self.grammar

  type item = Token.t


  let current self = self.token


  let advance self =
    { self with token = Lexer.read self.lexer }


  let init ?(grammar = Grammar.empty) ~lexer () =
    { lexer; grammar; token = Lexer.read lexer }
end


and Parser : (P.Type with type token = Token.t
                      and type state = State.t) = P.Make(Token)(State)


let (>>=) = Parser.(>>=)
let (>>)  = Parser.(>>)


let invalid_infix = fun _left ->
  Parser.token >>= fun tok ->
  Parser.error (Parser.With_message ("%s cannot be used in infix position" % Token.to_string tok))


let invalid_prefix =
  Parser.token >>= fun tok ->
  Parser.error (Parser.With_message ("%s cannot be used in prefix position" % Token.to_string tok))


let default_prefix =
  Parser.token >>= fun token ->
  Parser.consume token >> lazy (Parser.pure (Expr.atom token))


let juxtaposition left =
  prefix 90 >>= fun right ->
  let form_list =
    match left with
    | Form xs -> List.append xs [right]
    | atom    -> [atom; right] in
  Parser.pure (Form form_list)


let rec default_infix left =
  juxtaposition left


and infix rbp left =
  Parser.get >>= fun { State.grammar } ->
  Parser.token >>= fun token ->

  let name = Token.to_string token in

  let (parser, lbp) =
    Grammar.lookup_infix name grammar or (default_infix, 90) in
  (* print (" infix: tok = %s, rbp = %d, lbp = %d, left = %s" % (name, rbp, lbp, Expr.to_string left)); *)
  if lbp > rbp then
    parser left >>= infix rbp
  else
    Parser.pure left


and prefix precedence =
  Parser.get >>= fun { State.grammar } ->
  Parser.token >>= fun token ->

  let name = Token.to_string token in

  let parser = Grammar.lookup_prefix name grammar or default_prefix in
  (* print ("prefix: tok = %s, rbp = %d" % (name, precedence)); *)
  parser >>= fun left ->
  infix precedence left


let expression = prefix 0


let run parser ?grammar lexer =
  let state = State.init ?grammar ~lexer () in
  match Parser.run parser state with
  | Ok (expr, state') -> Ok expr
  | Error e -> Error (Parser.error_to_string e)


let parse ?grammar lexer =
  run expression ?grammar lexer

