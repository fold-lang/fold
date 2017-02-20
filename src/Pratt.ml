
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

  val define_syntax_rule : expr list -> t -> t

  val lookup_nud_rule : string -> t -> Parser.nud option
  val lookup_led_rule : string -> t -> Parser.led option

end = struct
  type t = Env of data * t option
  and data = {
    def : expr M.t;
    lbp : int M.t;
    nud : Parser.nud M.t;
    led : Parser.led M.t;
  }


  let empty =
    let data = {
      def = M.empty;
      lbp = M.empty;
      nud = M.empty;
      led = M.empty;
    } in
    Env (data, None)


  let key_for_rule rule =
    let apply_rule_type rt key =
      match rt with
      | `nud -> `nud key
      | `led -> `led key in

    let switch_rule_type rt =
      match rt with
      | `nud -> `led
      | `led -> `nud in

    let rec loop rule_type list =
      match list with
      | Atom (_, Symbol _) :: xs ->
        loop (switch_rule_type rule_type) xs

      | Atom (_, String key) :: _ ->
        Ok (apply_rule_type rule_type key)

      | _ ->
        Error "Invalid rule syntax"
    in
      loop `nud rule


  let define_nud_rule name rule (Env (data, next)) =
    print ("defining nud rule for name %s" % name);
    Env ({ data with nud = M.add name rule data.nud }, next)


  let define_led_rule name rule (Env (data, next)) =
    print ("defining led rule for name %s" % name);
    Env ({ data with led = M.add name rule data.led }, next)


  let rec lookup_nud_rule name (Env (data, next)) =
    match M.find name data.nud with
    | Some rule -> Some rule
    | None -> Option.(next >>= lookup_nud_rule name)


  let rec lookup_led_rule name (Env (data, next)) =
    match M.find name data.led with
    | Some rule -> Some rule
    | None -> Option.(next >>= lookup_led_rule name)


  (* TODO: Read the rule only once and have the `Parser.of_rule` return a parser. *)
  let define_syntax_rule rule self =
    let key = Result.force (key_for_rule rule) in
    match key with
    | `nud name -> define_nud_rule name (Parser.nud_of_rule rule) self
    | `led name -> define_led_rule name (Parser.led_of_rule rule) self
end

and Parser : sig
  include StateT
    with type 'a monad = 'a R.t
     and type state = State.t

  type nud = expr t
  type led = expr -> expr t

  val nud_of_rule : expr list -> nud
  val led_of_rule : expr list -> led

  val parse : Lexer.t -> Expr.t R.t
  val parse_string : Expr.t t -> string -> Expr.t R.t
end = struct
  include StateT(State)(R)

  type nud = Syntax.expr t
  type led = Syntax.expr -> Syntax.expr t


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
    let env  = State.env s in

    if (Precedence.lookup name or 90) > precedence then
      let parser =
        env
        |> Env.lookup_led_rule name
        |> Option.with_default parse_form in

      parser left >>= infix precedence
    else
      return left


  and prefix precedence =
    print ("prefix: precedence = %d" % precedence);
    get >>= fun s ->
    let name = Literal.to_string (Token.literal State.(s.token)) in
    let env  = State.env s in

    let parser =
      env
      |> Env.lookup_nud_rule name
      |> Option.with_default (parse_atom State.(s.token)) in

    parser >>= fun left ->
    infix precedence left


  and expression () =
    prefix 0


  let of_rule acc rule =
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

    loop [] acc rule


  let nud_of_rule rule =
    of_rule [] rule


  let led_of_rule rule =
    fun left -> of_rule [left] rule


  let parse lexer =
    let state = State.{
        env = Env.empty;
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

include Parser

