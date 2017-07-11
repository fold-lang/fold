open Pure
open Base
open Lex

module M = Map.Make(Token)

type error =
  | Empty
  | Unexpected_end   of { expected : token }
  | Unexpected_token of { expected : token; actual : token }
  | Failed_satisfy   of token
  | With_message     of string

let error_to_string e =
  match e with
  | Empty -> "empty"

  | Unexpected_end { expected } ->
    "expected `%s` but input terminated" % Token.show expected

  | Unexpected_token { expected; actual } ->
    "expected `%s` but got `%s`" % (Token.show expected, Token.show actual)

  | Failed_satisfy token ->
    "token `%s` did not satisfy predicate" % Token.show token

  | With_message msg ->
    msg

(* Base parser *)
type 'a parser = state -> ('a * state, error) result

and state =
  { lexer : Lexer.t;
    token : Token.t }

and 'a grammar = {
  name : string;
  data : 'a scope list;
  atom : Token.t -> 'a prefix;
  form : Token.t -> 'a infix option;
  join : 'a list -> 'a;
}

and 'a scope = {
  prefix : 'a prefix M.t;
  infix  : 'a infix  M.t;
}

and 'a prefix = ('a grammar ->       'a parser)
and 'a infix  = ('a grammar -> 'a -> 'a parser) * int

type 'a denotation =
  | Prefix of Token.t * 'a prefix
  | Infix  of Token.t * 'a infix


(* Monad instance *)

let pure x = fun s -> Ok (x, s)

let (>>=) p f =
  fun s ->
    match p s with
    | Ok (x, s') -> let p'= f x in p' s'
    | Error msg  -> Error msg


(* Alternative *)
let empty = fun g -> fun _state -> Error Empty

let (<|>) p1 p2 = fun state ->
  match p1 state with
  | Ok value -> Ok value
  | Error _  -> p2 state

let get       = fun s -> Ok (s, s)
let put s     = fun _ -> Ok ((), s)
let zero      = fun s -> Ok ((), s)

let combine p1 p2 =
  p1 >>= fun x ->
  p2 >>= fun y -> pure (x, y)


let default default p =
  p <|> pure default


let optional p =
  p >>= (fun () -> pure ()) |> default ()


let rec many p =
  default []
    (p >>= fun x ->
     many p >>= fun xs ->
     pure (x :: xs))


let rec some p =
  combine p (many p)


let error e =
  fun _state -> Error e

let state f =
  get >>= fun s ->
  let (a, s') = f s in
  put s' >>= fun () -> pure a


let modify f =
  state (fun s -> ((), f s))


let satisfy pred =
  get >>= fun {token} ->
  if pred token then pure token
  else error (Failed_satisfy token)


let expect expected =
  get >>= fun {token} ->
  match token with
  | actual when actual = expected -> pure actual
  | actual when actual = Lex.eof -> error (Unexpected_end { expected })
  | actual -> error (Unexpected_token { expected; actual })


let advance = fun state ->
  modify (fun state -> { state with token = Lexer.read state.lexer })
    state

let consume tok =
  expect tok >>= fun _ -> advance


let exactly x =
  expect x >>= fun x -> advance >>= fun () -> pure x


let any = fun state ->
  satisfy (const true)
    state


let one_of list =
  satisfy (fun x -> List.mem x list)


let none_of list =
  satisfy (fun x -> not (List.mem x list))


module Grammar = struct
  type 'a t = 'a grammar

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


  let invalid_prefix token =
    let parse g =
      let msg = if token = Lex.eof
        then "unexpected end of input"
        else "%s cannot be used in prefix position" % Token.to_string token in
      (error (With_message msg)) in
    parse


  let invalid_infix ?(lbp = 0) token =
    let parse g left =
      let msg = "%s cannot be used in infix position" % Token.to_string token in
      (error (With_message msg)) in
    Some (parse, lbp)


  let empty = {
    name = "";
    data = [];
    atom = invalid_prefix;
    form = (fun token -> None);
    join = (fun xs -> fail "invalid join");
  }


  let empty_scope = {
    infix  = M.empty;
    prefix = M.empty;
  }


  let define_infix token pareselet self =
    let first, rest =
      match self.data with
      | [] -> empty_scope, []
      | first::rest -> first, rest in
    let first' = { first with infix  = M.add token pareselet first.infix } in
    { self with data = first'::rest }


  let define_prefix token pareselet self =
    let first, rest =
      match self.data with
      | [] -> empty_scope, []
      | first::rest -> first, rest in
    let first' = { first with prefix  = M.add token pareselet first.prefix } in
    { self with data = first'::rest }


  let define rule self =
    match rule with
    | Prefix (token, parselet) -> define_prefix token parselet self
    | Infix  (token, parselet) -> define_infix  token parselet self


  let lookup_prefix token self =
    let rec loop data =
      match data with
      | s :: rest -> Option.(M.find token s.prefix <|> lazy (loop rest))
      | [] -> None in
    loop self.data


  let lookup_infix token self =
    let rec loop data =
      match data with
      | s :: rest -> Option.(M.find token s.infix <|> lazy (loop rest))
      | [] -> None in
    loop self.data


  let push_scope scope self =
    { self with data = scope :: self.data }


  let new_scope self =
    push_scope empty_scope self


  let pop_scope self =
    match self.data with
    | [] -> self
    | _ :: rest -> { self with data = rest }


  let scope_of_list rules =
    List.fold_left
      (fun scope -> function
         | Prefix (token, parselet) -> { scope with prefix = M.add token parselet scope.prefix }
         | Infix  (token, parselet) -> { scope with infix  = M.add token parselet scope.infix  })
      empty_scope
      rules


  let init
      ?(join = (fun xs -> fail "invalid join"))
      ?(atom = invalid_prefix)
      ?(form = invalid_infix ~lbp:90)
      name rules =
    let scope = scope_of_list rules in
    { data = [scope]; atom; form; join; name }
    (* |> define_prefix Lex.eof (fun g -> error (With_message "unexpected end of file")) *)
    (* |> define_infix  Lex.eof ((fun g left -> error Empty), 0) *)
end



let run parser state =
  match parser state with
  | Ok (expr, _) -> Ok expr
  | Error e -> Error e

(* join :: List a -> a
 *
 * - parse many
 *   - get prefix for token
 *   - parse prefix
 *   -
 *

let rec many p =
  default []
    (p >>= fun x ->
     print "y";
     many p >>= fun xs ->
     print "n";
     pure (x :: xs))

 * *)

(*

a       ==> a
f a     ==> (f a)
a + b   ==> (+ a b)
f a + b ==> (+ (f a) b)

*)
(* until (fun { token } -> has_infix token grammar) p *)


(* Lookup on advance to reduce cost *)
let has_infix grammar token =
  is_some Option.(Grammar.lookup_infix token grammar <|> lazy (grammar.form token))


let unless pred p =
  get >>= fun { token } ->
  if pred token then p
  else error (Failed_satisfy token)


let rec until pred p =
  get >>= fun { token } ->
  if pred token then
    p >>= fun x ->
    until pred p >>= fun xs ->
    pure (x :: xs)
  else pure []


let parse_prefix grammar =
  get >>= fun { token } ->
  let rule = Grammar.lookup_prefix token grammar or lazy (grammar.atom token) in
  rule grammar


let rec nud grammar rbp =
  until (fun token -> not (has_infix grammar token)) (parse_prefix grammar) >>= function
  | []  -> parse_prefix grammar >>= led grammar rbp  (* everything is a valid prefix *)
  | [x] -> led grammar rbp x
  | xs  -> led grammar rbp (grammar.join xs)


and led grammar rbp left =
  get >>= fun { token } ->
  match Option.(Grammar.lookup_infix token grammar <|> lazy (grammar.form token)) with
  | Some (rule, lbp) ->
    if lbp > rbp then
      rule grammar left >>= led grammar rbp
    else
      pure left
  | None -> error (With_message ("invalid infix: %s" % Token.to_string token))


let parse grammar =
  nud grammar 0


let singleton x g =
  advance >>= fun () ->
  pure x


let delimiter str =
  let parse g _ = error (With_message "unexpected delimiter") in
  Infix (`Symbol str, (parse, 0))


let infix precedence str f =
  let p g x =
    advance >>= fun () ->
    nud g precedence >>= fun y ->
    pure (f x y) in
  Infix (`Symbol str, (p, precedence))

let infixr precedence str f =
  let p g x =
    advance >>= fun () ->
    nud g (precedence - 1) >>= fun y ->
    pure (f x y) in
  Infix (`Symbol str, (p, precedence))


let prefix str f =
  let p g =
    advance >>= fun () ->
    parse g >>= fun x ->
    pure (f x) in
  Prefix (`Symbol str, p)


let postfix precedence str f =
  let p g x =
    advance >>= fun () ->
    pure (f x) in
  Infix (`Symbol str, (p, precedence))


let between s e f =
  let p g =
    advance >>= fun () ->
    parse g >>= fun x ->
    consume (`Symbol e) >>= fun () ->
    pure (f x) in
  Prefix (`Symbol s, p)


 let concat token f =
   let precedence = 90 in
   let parse g x =
     nud g precedence >>= fun y ->
     pure (f x y) in
   (parse, precedence)

