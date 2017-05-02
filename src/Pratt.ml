open Pure
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

    type rule =
      | Prefix of prefix
      | Infix of infix

    val init : atom: (Token.t -> prefix) -> ?form: (Token.t -> infix) -> (Token.t * rule) list -> t

    val define : Token.t -> rule -> t -> t

    val prefix : Token.t -> t -> prefix
    val infix  : Token.t -> t -> infix

    val invalid_infix  : ?lbp: int -> Token.t -> infix
    val invalid_prefix : Token.t -> prefix

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
      atom   : Token.t -> prefix;
      form   : Token.t -> infix;
    }


    let invalid_infix ?(lbp = 0) token =
      let parse left =
        let msg = "%s cannot be used in infix position" % Token.to_string token in
        Parser.(error (With_message msg)) in
      (parse, lbp)


    let invalid_prefix token =
      let msg = if token = Lex.eof
        then "unexpected end of file"
        else "%s cannot be used in prefix position" % Token.to_string token in
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
        form   = fun tok -> invalid_infix ~lbp:90 tok;
      } in
      List.fold_left
        (fun self (token, rule) -> define token rule self)
        (* { empty with atom } *)
        empty
        scope
      |> define Lex.eof (Prefix (invalid_prefix Lex.eof))
      |> define Lex.eof (Infix  (invalid_infix  Lex.eof))


    let prefix token self =
      let rec loop {prefix; next} =
        match M.find token prefix with
        | None -> Option.(next >>= loop)
        | some -> some in
      loop self or self.atom token


    let infix token self =
      let rec loop {infix; next} =
        match M.find token infix with
        | None -> Option.(next >>= loop)
        | some -> some in
      loop self or self.form token
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


  let rec prefix rbp =
    Parser.get >>= fun { State.grammar } ->
    Parser.token >>= fun token ->

    let parse = Grammar.prefix token grammar in
    log ("nud: tok = %s, rbp = %d" % (Token.to_string token, rbp));
    parse >>= fun left ->
    infix rbp left


  and infix rbp left =
    Parser.get >>= fun { State.grammar } ->
    Parser.token >>= fun token ->

    let (parse, lbp) = Grammar.infix token grammar in
    log ("led: tok = %s, rbp = %d, lbp = %d, left = %s" % (Token.to_string token, rbp, lbp, Expr.to_string left));
    if lbp > rbp then
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


  let singleton x =
    Parser.advance >>= fun () ->
    Parser.pure x

  let delimiter =
    let parse _ = Parser.error (Parser.With_message "unexpected delimiter") in
    Grammar.Infix (parse, 0)

  let infix precedence f =
    let parse x =
      Parser.advance >>= fun () ->
      prefix precedence >>= fun y ->
      Parser.pure (f x y) in
    Grammar.Infix (parse, precedence)


  let prefix f =
    let parse =
      Parser.advance >>= fun () ->
      expression >>= fun x ->
      Parser.pure (f x) in
    Grammar.Prefix parse


  let postfix precedence f =
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





