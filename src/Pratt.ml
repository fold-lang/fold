
open Pure
open Syntax
open Lex

module Option = struct
  include Option
  let map f = function Some x -> Some (f x) | None -> None
end


module M = struct
  include Map.Make(String)

  let find k m =
    Option.catch (fun () -> find k m)
end


module Precedence = struct
  let delimiter   =  0
  let keyword     =  1
  let assignment  = 10
  let conditional = 20
  let sum         = 30
  let product     = 40
  let exponent    = 50
  let prefix      = 60
  let postfix     = 70
  let call        = 80
  let group       = 80
  let terminal    = 90

  let lookup name =
    match name with
    (* Match atomic symbols. *)
    | "<EOF>" -> delimiter
    | ";" -> 20
    (* Match symbols that can start an operator. *)
    | str ->
      begin match str.[0] with
      | '=' -> assignment
      | '#' -> conditional
      | '+' | '-' -> sum
      | '*' | '/' -> product
      | '(' | '{' | '[' -> group
      | _ -> 30 (* default non symbolic? XXX *)
    end
end

module rec State : sig
  type t = {
    lexer : Lex.Lexer.t;
    token : Lex.Token.t;
    env   : Env.t;
  }

  val env : t -> Env.t
end = struct
  type t = {
    lexer : Lex.Lexer.t;
    token : Lex.Token.t;
    env   : Env.t;
  }

  let env self = self.env
end

and Env : sig
  type t

  val empty : t

  val default : t

  val dump : t -> unit

  val new_scope : t -> t

  val pop_scope : t -> t

  val define_syntax : string -> Expr.t Parselet.t -> t -> t

  val lookup_prefix : string -> t -> Expr.t Parselet.prefix option
  val lookup_infix  : string -> t -> Expr.t Parselet.infix  option

end = struct
  type t = {
    data   : expr M.t;
    prefix : Expr.t Parselet.prefix M.t;
    infix  : Expr.t Parselet.infix M.t;
    next   : t option;
  }


  let empty = {
    data   = M.empty;
    infix  = M.empty;
    prefix = M.empty;
    next   = None;
  }


  let new_scope self =
    { empty with next = Some self }


  let pop_scope self =
    self.next or self


  let dump self =
    print "INFIX:";
    M.iter (fun k (p, prec) -> print ("%s => <infix %d>" % (k, prec))) self.infix;
    print "";
    print "PREFIX:";
    M.iter (fun k p -> print ("%s => <prefix>" % k)) self.prefix;
    print ""


  let define_syntax name parselet self =
    match parselet with
    | Parselet.Infix  p ->
      { self with infix  = M.add name p self.infix  }

    | Parselet.Prefix p ->
      { self with prefix = M.add name p self.prefix }


  let rec lookup_prefix name { prefix; next } =
    match M.find name prefix with
    | Some rule ->
      Some rule

    | None ->
      Option.(next >>= lookup_prefix name)


  let rec lookup_infix name { infix; next } =
    match M.find name infix with
    | Some rule ->
      Some rule

    | None ->
      Option.(next >>= lookup_infix name)


  let default =
    empty
    |> define_syntax "<EOF>"  (Parselet.Infix  ((fun x -> undefined ()), 0))
    |> define_syntax "<EOF>"  (Parselet.Prefix Parser.invalid_prefix)

end

and Parser : sig
  include StateT
    with type 'a monad = ('a, string) result
     and type state = State.t

  include Functor with type 'a t := 'a t


  (* Alternative instance *)

  val empty : 'a t
  (** [empty] is a parser that successfully matches empty input. *)

  val (<|>) : 'a t -> 'a t -> 'a t
  (** [p1 <|> p2] is a parser built by alternative composition of [p1] and
      [p2]. The result of [p1] is returned in case it succeeds, or [p2]
      otherwise *)

  val combine : 'a t -> 'b t -> ('a * 'b) t

  val infix  : int -> Expr.t -> Expr.t Parser.t
  val prefix : int -> Expr.t Parser.t

  val expression : unit -> Expr.t Parser.t

  val invalid_infix  : Expr.t -> Expr.t Parser.t

  val invalid_prefix : Expr.t Parser.t

  val consume : Literal.t -> unit t

  val expect : Literal.t -> Expr.t t

  val advance : unit t

  val parse : ?env: Env.t -> Lexer.t -> (Expr.t, string) Result.t
  val run_parser : ?env: Env.t -> Expr.t t -> Lexer.t -> (Expr.t, string) Result.t
  val run_parser_with_string : Expr.t t -> string -> (Expr.t, string) Result.t
end = struct
  module StateT      = StateT(State)(Result.Of_error(String))
  module Functor     = Functor.Of_monad(StateT)
  module Applicative = Applicative.Of_monad(StateT)

  include StateT
  include Functor
  include Applicative

  (* Alternative insance *)
  let empty = fun _ -> Error "empty"


  let (<|>) p1 p2 =
    fun state ->
      match p1 state with
      | Error _  -> p2 state
      | Ok value -> Ok value


  let rec some p =
    p >>= fun x  -> some p
      >>= fun xs -> pure (x :: xs)


  let many p =
    some p <|> empty


  let error msg = fun _ -> Error msg


  let combine p1 p2 =
    p1 >>= fun x ->
    p2 >>= fun y -> pure (x, y)

  let optional p =
    p <|> empty

  let satisfy predicate =
    get >>= fun { State.token } ->
    if predicate token then
       pure (Expr.atom token)
    else
       error "could not satisfy test"

  let any = satisfy (const true)

  let a x = satisfy ((=) x)
  let an  = a

  let (<?>) p label s =
    match p s with
    | Error _ ->
      let msg =
        if Token.literal State.(s.token) = Literal.eof then
          "%s unexpected end of file while reading %s" %
          (Location.show (Token.location State.(s.token)), label)
        else
        if label = (Literal.show (Symbol "EOF")) then
          "parsing stopped at %s" % Token.to_string State.(s.token)
        else
          "expected `%s` but `%s` found" % (label, Token.to_string State.(s.token)) in
      Error msg
    | Ok x -> Ok x


  let expect literal =
    a literal <?> Token.to_string literal


  let advance =
    modify begin fun s ->
      let token = Lexer.next s.State.lexer in
      State.{ s with token }
    end


  let consume literal =
    expect literal >> lazy advance


  let inspect f =
    get >>= fun s ->
    f s; put s


  let inspect_token =
    inspect begin fun { State.token } ->
      print ("token: %s" % Lex.Token.show token)
    end


  let invalid_infix _left =
    get >>= fun { State.token } ->
    error ("%s cannot be used in infix position" % Token.to_string token)


  let invalid_prefix =
    get >>= fun { State.token } ->
    error ("%s cannot be used in prefix position" % Token.to_string token)


  let rec parse_atom =
    get >>= fun { State.token } ->
    consume (Token.literal token) >> lazy (pure (Expr.atom token))


  let rec parse_form left =
    prefix 90 >>= fun right ->
    let form_list =
      match left with
      | Form xs -> List.append xs [right]
      | atom    -> [atom; right] in
    pure (Form form_list)


  and infix rbp left =
    get >>= fun s ->

    let name = Literal.to_string (Token.literal State.(s.token)) in
    let env  = State.env s in

    let parse, lbp =
      Env.lookup_infix name env or (parse_form, 90)
    in
      print (" infix: tok = %s, rbp = %d, lbp = %d, left = %s" % (name, rbp, lbp, Expr.to_string left));
      if lbp > rbp then
        parse left >>= infix rbp
      else
        pure left


  and prefix precedence =
    get >>= fun s ->

    let name = Literal.to_string (Token.literal State.(s.token)) in
    let env  = State.env s in

    let parse =
      env
      |> Env.lookup_prefix name
      |> Option.with_default parse_atom in

    print ("prefix: tok = %s, rbp = %d" % (name, precedence));

    parse >>= fun left ->
    infix precedence left


  and expression () = prefix 0


  let parse ?(env : Env.t = Env.default) (lexer : Lexer.t) =
    let e : Env.t = env in
    let state = State.{
        env = e;
        lexer;
        token = Lexer.read lexer
      } in
    match run (expression ()) state with
    | Ok (expr, _) -> Ok expr
    | Error msg    -> Error msg


  let run_parser ?(env : Env.t = Env.default) p lexer =
    let e : Env.t = env in
    let state = State.{
        env = e;
        lexer;
        token = Lexer.read lexer
      } in
    match run p state with
    | Ok (expr, _) -> Ok expr
    | Error e -> Error e


  let run_parser_with_string p str =
    let e : Env.t = Env.default in
    let lexer = Lexer.from_string str in
    let state = State.{
        env = e;
        lexer;
        token = Lexer.read lexer
      } in
    match run p state with
    | Ok (expr, _) -> Ok expr
    | Error e -> fail e
end


and Parselet : sig
  type 'a prefix = 'a Parser.t

  type 'a infix = ('a -> 'a Parser.t) * int

  type 'a t =
    | Prefix of 'a prefix
    | Infix  of 'a infix


  val create : expr list -> (string * expr t) list
  (** [of_rule rule] is a result with a parselet generated based on the
      syntax [rule].

      Expressions in the rule define the type of the parselet (prefix or infix).

      A valid rule must:

      - have a string token (representing a keyword) on the first or second position;
      - (?) not have consecutive symbol tokens (representing expressions);

      Here is the list of some valid rules:

      {[
        ["-", a]
        ["let", "rec", name, "=", expr, "in", body]
        [a, "+", b]
      ]}


      Here is the list of some invalid rules:

      {[
        [a, b]
        (?) ["unless", test, body]
        [a]
        ["hey"]
        []
      ]}
   *)
end = struct
  type 'a prefix = 'a Parser.t

  type 'a infix = ('a -> 'a Parser.t) * int

  type 'a t =
    | Prefix of 'a prefix
    | Infix  of 'a infix


  let infix_delimiter = Infix (Parser.invalid_infix, Precedence.delimiter)


  let delimiters list =
    List.fold_left
      begin fun r a ->
        match a with
        | Atom (String x) -> (x, infix_delimiter) :: r
        | _ -> r
      end
      []
      list


  let (>>=) = Parser.(>>=)
  let (>>)  = Parser.(>>)

  let keyword name =

    let p left =
      print ("consuming %s, have left = %s" % (name, Expr.to_string left));
      Parser.consume (Symbol name) >> lazy begin
        print "parsing kw expr";
        Parser.infix 0 (Expr.symbol name) >>= fun kw_expr ->
        print ("parselet/left = %s" % Expr.to_string left);
        Parser.pure Expr.(form [symbol ";;"; left; kw_expr])
      end
    in
      (name, Infix (p, Precedence.keyword))


  let create rule =
    let rec go name args list =
      match list with
      | [] ->
        Parser.pure (Form (Expr.symbol name :: List.rev args))

      | Atom (Symbol x) :: rest ->
        Parser.prefix Precedence.keyword >>= fun e ->
        go name (e :: args) rest

      | Atom (String x) :: rest ->
        Parser.consume (Symbol x) >> lazy (go name args rest)

      | _ ->
        invalid_arg "invalid rule definition syntax"
    in
      match rule with
      | Atom (String name) :: xs ->
        Parser.(name, Prefix (consume (Symbol name) >> lazy (go name [] xs)))
        :: keyword name
        :: delimiters xs

      | Atom _ :: Atom (String name) :: xs ->
        let p left =
          Parser.(consume (Symbol name) >> lazy (go name [left] xs)) in
        let precedence =
          Precedence.lookup name in
        (name, Infix (p, precedence))
        :: (delimiters xs)
      | _ -> []
end


include Parser

