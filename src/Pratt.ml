
open Pure
open Base
open Syntax
open Lex

let debug = ref false

let log =
  let ignore ?file ?terminator ?flush x = () in
  if !debug then print else ignore


module M = struct
  include Map.Make(Token)

  let find k m =
    Option.catch (fun () -> find k m)
end

module P = Parser


module Make(Expr : sig type t val to_string : t -> string end) = struct

  module rec Grammar : sig
    type t

    type prefix = Expr.t Parser.t
    type infix  = (Expr.t -> Expr.t Parser.t) * int

    val init : ?form: (Token.t -> infix) -> ?atom: (Token.t -> prefix) -> unit -> t

    val lookup_infix  : Token.t -> t -> infix
    val lookup_prefix : Token.t -> t -> prefix

    val define_infix  : Token.t -> infix -> t -> t
    val define_prefix : Token.t -> prefix -> t -> t

    val invalid_infix  : ?lbp: int -> Token.t -> infix
    val invalid_prefix : Token.t -> prefix

    val new_scope : t -> t
    val pop_scope : t -> t

    val dump : t -> unit
  end = struct
    type prefix = Expr.t Parser.t
    type infix  = (Expr.t -> Expr.t Parser.t) * int

    type scope = {
      prefix : prefix M.t;
      infix  : infix  M.t;
    }

    type t = {
      data : scope list;
      atom : Token.t -> prefix;
      form : Token.t -> infix;
    }

    let dump self =
      let string_of_prefix _ = "<prefix>" in
      let string_of_infix (_, p) = "<infix %d>" % p in
      List.iteri (fun i { prefix; infix } ->
        print ("prefix [%d]:" % i);
        M.iter (fun k v -> print ("%s => %s" % (Token.to_string k, string_of_prefix v))) prefix;
        print ("infix[%d]:" % i);
        M.iter (fun k v -> print ("%s => %s" % (Token.to_string k, string_of_infix v))) infix;
        print "")
        self.data


    let invalid_infix ?(lbp = 0) token =
      let parse left =
        let msg = "%s cannot be used in infix position" % Token.to_string token in
        Parser.(error (With_message msg)) in
      (parse, lbp)


    let invalid_prefix token =
      let msg = if token = Lex.eof
        then "unexpected end of input"
        else "%s cannot be used in prefix position" % Token.to_string token in
      Parser.(error (With_message msg))


    let empty = {
      data = [];
      atom = invalid_prefix;
      form = invalid_infix ~lbp:90;
    }


    let empty_scope = {
      infix  = M.empty;
      prefix = M.empty;
    }


    let define_infix token rule self =
      let first, rest =
        match self.data with
        | [] -> empty_scope, []
        | first::rest -> first, rest in
      let first' = { first with infix  = M.add token rule first.infix } in
      { self with data = first'::rest }


    let define_prefix token rule self =
      let first, rest =
        match self.data with
        | [] -> empty_scope, []
        | first::rest -> first, rest in
      let first' = { first with prefix  = M.add token rule first.prefix } in
      { self with data = first'::rest }


    let init ?(form = invalid_infix ~lbp:90) ?(atom = invalid_prefix) () =
      { data = []; atom; form }
      |> define_prefix Lex.eof (invalid_prefix Lex.eof)
      |> define_infix  Lex.eof (invalid_infix  Lex.eof)


    let lookup_prefix token self =
      let rec loop data =
        match data with
        | s :: rest -> Option.(M.find token s.prefix <|> lazy (loop rest))
        | [] -> None in
      loop self.data or lazy (self.atom token)


    let lookup_infix token self =
      let rec loop data =
        match data with
        | s :: rest -> Option.(M.find token s.infix <|> lazy (loop rest))
        | [] -> None in
      loop self.data or lazy (self.form token)


    let new_scope self =
      { self with data = empty_scope :: self.data }


    let pop_scope self =
      match self.data with
      | [] -> self
      | _ :: rest -> { self with data = rest }
  end

  and State : sig
    type t = {
      lexer   : Lexer.t;
      grammar : Grammar.t;
      token   : Token.t option;
    }

    include P.Input with type t := t
                     and type item = Token.t

    val init : grammar: Grammar.t -> lexer: Lexer.t -> unit -> t
  end = struct
    type t = {
      lexer   : Lexer.t;
      grammar : Grammar.t;
      token   : Token.t option;
    }

    type item = Token.t


    let current self = self.token


    let advance self =
      { self with token = Lexer.read self.lexer }


    let init ~grammar ~lexer () =
      { lexer; grammar; token = Lexer.read lexer }
  end


  and Parser : (P.Type with type token = Token.t
                        and type state = State.t) = P.Make(Token)(State)

  let (>>=) = Parser.(>>=)
  let (>>)  = Parser.(>>)


  let rec parse_prefix rbp =
    Parser.get >>= fun { State.grammar } ->
    Parser.token >>= fun token ->

    let parse = Grammar.lookup_prefix token grammar in
    log ("nud: tok = %s, rbp = %d" % (Token.to_string token, rbp));
    parse >>= fun left ->
    parse_infix rbp left


  and parse_infix rbp left =
    Parser.get >>= fun { State.grammar } ->
    Parser.token >>= fun token ->

    let (parse, lbp) = Grammar.lookup_infix token grammar in
    log ("led: tok = %s, rbp = %d, lbp = %d, left = %s, stop = %b" %
         (Token.to_string token, rbp, lbp, Expr.to_string left, lbp <= rbp));
    if lbp > rbp then
      parse left >>= parse_infix rbp
    else
      Parser.pure left


  let expression = parse_prefix 0


  let run parser ~grammar lexer =
    let state = State.init ~grammar ~lexer () in
    match Parser.run parser state with
    | Ok (expr, _) -> Ok expr
    | Error e -> Error (Parser.error_to_string e)

  (* val prefix    : string -> (Expr.t -> Expr.t) -> t -> t *)
  (* val infix     : int -> string -> (Expr.t -> Expr.t -> Expr.t) -> t -> t *)
  (* val infixr    : int -> string -> (Expr.t -> Expr.t -> Expr.t) -> t -> t *)
  (* val postfix   : int -> string -> (Expr.t -> Expr.t) -> t -> t *)
  (* val between   : string -> string -> (Expr.t -> Expr.t) -> t -> t *)
  (* val delimiter : string -> t -> t *)

  let singleton x =
    Parser.advance >>= fun () ->
    Parser.pure x


  let delimiter str =
    let parse _ = Parser.error (Parser.With_message "unexpected delimiter") in
    Grammar.define_infix (Symbol str) (parse, 0)


  let infix precedence str f =
    let parse x =
      Parser.advance >>= fun () ->
      parse_prefix precedence >>= fun y ->
      Parser.pure (f x y) in
    Grammar.define_infix (Symbol str) (parse, precedence)


  let infixr precedence str f =
    let parse x =
      Parser.advance >>= fun () ->
      parse_prefix (precedence - 1) >>= fun y ->
      Parser.pure (f x y) in
    Grammar.define_infix (Symbol str) (parse, precedence)


  let prefix str f =
    let parse =
      Parser.advance >>= fun () ->
      expression >>= fun x ->
      Parser.pure (f x) in
    Grammar.define_prefix (Symbol str) parse


  let postfix precedence str f =
    let parse x =
      Parser.advance >>= fun () ->
      Parser.pure (f x) in
    Grammar.define_infix (Symbol str) (parse, precedence)


  let between s e f g =
    let prefix =
      Parser.advance >>= fun () ->
      expression >>= fun x ->
      Parser.consume (Symbol e) >>= fun () ->
      Parser.pure (f x) in
    g
    |> Grammar.define_prefix (Symbol s) prefix
    |> delimiter e
end

