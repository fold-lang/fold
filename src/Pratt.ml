
open Pure
open Syntax
open Lex

module Option = struct
  include Option
  let map f = function Some x -> Some (f x) | None -> None
end


module Result = struct
  include Result
  let force = function Ok x -> x | _ -> undefined ()
end


module M = struct
  include Map.Make(String)

  let find k m =
    Option.catch (fun () -> find k m)
end


module R = Result.Of_error(String)



module Precedence = struct
  let assignment  = 10
  let conditional = 20
  let sum         = 30
  let product     = 40
  let exponent    = 50
  let prefix      = 60
  let postfix     = 70
  let call        = 80
  let terminal    = 90

  let lookup name =
    match name with
    (* Match atomic symbols. *)
    | "<EOF>" -> 0
    | ";" -> 20
    (* Match symbols that can start an operator. *)
    | str ->
      begin match str.[0] with
      | '=' -> 10
      | '#' -> 20
      | '+' | '-' -> 30
      | '*' | '/' -> 40
      | '(' | '{' | '[' -> 80
      | _ -> 30 (* default non symbolic? XXX *)
    end
end

module rec State : sig
  type t = {
    lexer   : Lex.Lexer.t;
    token   : Lex.Token.t;
    env : Env.t;
  }

  val env : t -> Env.t
end = struct
  type t = {
    lexer   : Lex.Lexer.t;
    token   : Lex.Token.t;
    env : Env.t;
  }

  let env self = self.env
end

and Env : sig
  type t

  val empty : t

  val dump : t -> unit

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
end

and Parser : sig
  include StateT
    with type 'a monad = 'a R.t
     and type state = State.t

  val expression : unit -> Expr.t Parser.t

  val consume : Literal.t -> unit t
  val advance : unit t

  val parse : Env.t -> Lexer.t -> Expr.t R.t
  val parse_string : Expr.t t -> string -> Expr.t R.t
end = struct
  include StateT(State)(R)

  type prefix = { parser : expr t}
  type infix  = { parser : expr -> expr t; precedence : int }

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
    print ("testing token %s" % Token.to_string token);
    if test (Token.literal token) then
      (print "ok";
       return (Expr.atom token))
    else
      (print "no";
       error "could not satisfy test")

  let any        = satisfy (const true)
  let exactly x  = satisfy (fun lit -> print ("comparing %s and %s" % (Literal.to_string lit, Literal.to_string x)); lit = x)
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
      let token = Lexer.next s.State.lexer in
      State.{ s with token }
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


  let rec parse_atom =
    get >>= fun State.{ token } ->
    consume (Token.literal token) >> lazy (return (Expr.atom token))


  let rec parse_form left =
    prefix 90 >>= fun right ->
    let form_list =
      match left with
      | Form xs -> List.append xs [right]
      | atom    -> [atom; right] in
    return (Form form_list)


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
        return left


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


  let parse (env : Env.t) (lexer : Lexer.t) =
    let e : Env.t = env in
    let state = State.{
        env = e;
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
end


and Parselet : sig
  type 'a prefix = 'a Parser.t

  type 'a infix = ('a -> 'a Parser.t) * int

  type 'a t =
    | Prefix of 'a prefix
    | Infix  of 'a infix


  val create : expr list -> (string * expr t, string) result
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


  let create rule =
    let rec go fname res list =
      print ("rule: %s" % (Expr.to_string (Form list)));
      match list with
      | [] ->
        Parser.pure (Form (Expr.symbol fname :: List.rev res))

      | Atom (_loc, Symbol x) :: rest ->
        print ("parselet: parsing symbol %s" % x);
        Parser.(expression () >>= fun e ->
          go fname (e :: res) rest)

      | Atom (_loc, (String x as lit)) :: rest ->
        print ("parselet: parsing string %s" % x);
        Parser.(consume lit >> lazy (go fname res rest))

      | _ ->
        invalid_arg "invalid rule definition syntax"
    in
      match rule with
      | Atom (_, (String s)) :: xs ->
        let parselet =
          Parser.(consume (Symbol s) >> lazy (go s [] xs))
        in
          Ok (s, Prefix parselet)

      | Atom (_, _) :: Atom (_, (String s)) :: xs ->
        let parse left =
          Parser.(consume (Symbol s) >> lazy (go s [left] xs)) in

        let precedence =
          Precedence.lookup s
        in
          Ok (s, Infix (parse, precedence))

      | _ -> Error "Invalid rule syntax"

end


include Parser


