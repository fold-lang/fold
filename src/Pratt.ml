open Pure
open Syntax
open Lex


module M = struct
  include Map.Make(Token)

  let find k m =
    Option.catch (fun () -> find k m)
end

module P = Parser


module Make(Expr : Pure.Type) = struct

  module rec Grammar : sig
    type t

    type prefix = Expr.t Parser.t
    type infix  = (Expr.t -> Expr.t Parser.t) * int

    type rule =
      | Prefix of prefix
      | Infix of infix

    val init : atom: prefix -> ?form: infix -> (Token.t * rule) list -> t

    val define : Token.t -> rule -> t -> t

    val prefix : Token.t -> t -> prefix
    val infix  : Token.t -> t -> infix
  end = struct
    type prefix = Expr.t Parser.t
    type infix  = (Expr.t -> Expr.t Parser.t) * int

    type rule =
      | Prefix of prefix
      | Infix of infix

    type t = {
      prefix : prefix M.t;
      infix  : infix  M.t;
      next   : t option;
      atom   : prefix;
      form   : infix;
    }


    let invalid_infix token =
      let parse left =
        let msg = "%s cannot be used in infix position" % Token.to_string token in
        Parser.(error (With_message msg)) in
      (parse, 0)


    let invalid_prefix token =
      let msg = "%s cannot be used in prefix position" % Token.to_string token in
      Parser.(error (With_message msg))


    let define token rule self =
      match rule with
      | Prefix x -> { self with prefix = M.add token x self.prefix }
      | Infix  x -> { self with infix  = M.add token x self.infix  }


    let init ~atom ?form scope =
      let empty = {
        prefix = M.empty;
        infix  = M.empty;
        next   = None;
        atom;
        form   = invalid_infix  (Symbol "__xxx__");
      } in
      List.fold_left
        (fun self (token, rule) -> define token rule self)
        (* { empty with atom } *)
        empty
        scope
      |> define Lex.eof (Prefix (invalid_prefix Lex.eof))
      |> define Lex.eof (Infix  (invalid_infix Lex.eof))


    let prefix token self =
      let rec loop {prefix; next} =
        match M.find token prefix with
        | None -> Option.(next >>= loop)
        | some -> some in
      loop self or self.atom


    let infix token self =
      let rec loop {infix; next} =
        match M.find token infix with
        | None -> Option.(next >>= loop)
        | some -> some in
      loop self or self.form
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


  let rec prefix precedence =
    Parser.get >>= fun { State.grammar } ->
    Parser.token >>= fun token ->

    let parse = Grammar.prefix token grammar in
    parse >>= fun left ->
    infix precedence left


  and infix rbp left =
    Parser.get >>= fun { State.grammar } ->
    Parser.token >>= fun token ->

    let (parse, precedence) = Grammar.infix token grammar in
    if precedence > rbp then
      parse left >>= infix rbp
    else
      Parser.pure left


  let expression =
    prefix 0


  let run parser ~grammar lexer =
    let state = State.init ~grammar ~lexer () in
    match Parser.run parser state with
    | Ok (expr, _) -> Ok expr
    | Error e -> Error (Parser.error_to_string e)


  let parse ~grammar lexer =
    run expression ~grammar lexer


  let atom f =
    Parser.token >>= fun token ->
    Parser.advance >>= fun () ->
    Parser.pure (f token)


  let binary_infix precedence f =
    let parse x =
      Parser.advance >>= fun () ->
      prefix precedence >>= fun y ->
      Parser.pure (f x y) in
    Grammar.Infix (parse, precedence)


  let unary_prefix f =
    let parse =
      Parser.advance >>= fun () ->
      expression >>= fun x ->
      Parser.pure (f x) in
    Grammar.Prefix parse


  let unary_postfix precedence f =
    let parse x =
      Parser.advance >>= fun () ->
      Parser.pure (f x) in
    Grammar.Infix (parse, precedence)


  let group e =
    let parse =
      Parser.advance >>= fun () ->
      expression >>= fun x ->
      Parser.consume e >>= fun () ->
      Parser.pure x in
    Grammar.Prefix parse
end





