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

  type t = {
    prefix : prefix M.t;
    infix  : infix  M.t;
    next   : t option;
  }

  val empty : t

  val lookup_prefix : string -> t -> prefix option
  val lookup_infix : string -> t -> infix option
end = struct
  type prefix = expr Parser.t
  type infix  = (expr -> expr Parser.t) * int

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
  }

  include P.Input with type t := t
                   and type item = Token.t

  val init : ?grammar: Grammar.t -> lexer: Lexer.t -> unit -> t
end = struct
  type t = {
    lexer   : Lexer.t;
    grammar : Grammar.t;
  }

  type item = Token.t

  let next self =
    match Lexer.next self.lexer with
    | Some token -> Some (token, self)
    | None -> None

  let init ?(grammar = Grammar.empty) ~lexer () =
    { lexer; grammar }
end


and Parser : (P.Type with type token = Token.t
                      and type state = State.t) = P.Make(Token)(State)


let (>>=) = Parser.(>>=)
let (>>)  = Parser.(>>)


let invalid_infix () = fun _left ->
  Parser.token >>= fun tok ->
  Parser.error (Parser.With_message ("%s cannot be used in infix position" % Token.to_string tok))


let invalid_prefix () =
  Parser.token >>= fun tok ->
  Parser.error (Parser.With_message ("%s cannot be used in prefix position" % Token.to_string tok))


let rec parse_atom =
  Parser.token >>= fun tok -> Parser.consume tok >> lazy (Parser.pure (Expr.atom tok))


let rec parse_form left =
  prefix 90 >>= fun right ->
  let form_list =
    match left with
    | Form xs -> List.append xs [right]
    | atom    -> [atom; right] in
  Parser.pure (Form form_list)


and infix rbp left =
  Parser.get >>= fun { State.grammar } ->
  Parser.token >>= fun token ->

  let name = Token.to_string token in

  let (parser, lbp) =
    Grammar.lookup_infix name grammar or (parse_form, 90) in
  if lbp > rbp then
    parser left >>= infix rbp
  else
    Parser.pure left


and prefix precedence =
  Parser.get >>= fun { State.grammar } ->
  Parser.token >>= fun token ->

  let name = Token.to_string token in

  let parser = Grammar.lookup_prefix name grammar or parse_atom in
  parser >>= fun left ->
  infix precedence left


let expression () = prefix 0


let parse lexer =
  let state = State.init ~lexer () in
  let parser = expression () in
  match Parser.run parser state with
  | Ok (expr, state') -> Ok expr
  | Error e -> Error (Parser.error_to_string e)

