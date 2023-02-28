let const x _ = x
let ( << ) f g x = f (g x)

let is_some = function
  | Some _ -> true
  | None -> false

let flip f x y = f y x

(* TODO: When failing show the so-far parsed result. *)

module type Lexer = sig
  type t
  type token

  val pp_token : Format.formatter -> token -> unit
  val compare_token : token -> token -> int
  val next : t -> token option
  val pick : t -> token option
end

module Make (Lexer : Lexer) = struct
  module Table = struct
    include Map.Make (struct
      type t = Lexer.token

      let compare = Lexer.compare_token
    end)

    let get tbl x = try Some (find tbl x) with Not_found -> None
  end

  type token = Lexer.token

  let pp_token = Lexer.pp_token

  type error =
    | Unexpected of { expected : token option; actual : token option }
    | Invalid_infix of token
    | Invalid_prefix of token
    | Zero

  let unexpected_token ?expected actual =
    Unexpected { expected; actual = Some actual }

  let unexpected_end ?expected () = Unexpected { expected; actual = None }
  let invalid_prefix t = Invalid_prefix t
  let invalid_infix t = Invalid_infix t

  let error_to_string = function
    | Unexpected { expected = Some t1; actual = Some t2 } ->
      Fmt.str "Syntax error: expected '%a' but got '%a'" pp_token t1 pp_token t2
    | Unexpected { expected = Some t; actual = None } ->
      Fmt.str "Syntax error: unexpected end of file while parsing '%a'" pp_token
        t
    | Unexpected { expected = None; actual = None } ->
      Fmt.str "Syntax error: unexpected end of file"
    | Unexpected { expected = None; actual = Some t } ->
      Fmt.str "Syntax error: unexpected token '%a'" pp_token t
    | Invalid_infix token ->
      Fmt.str "Syntax error: '%a' cannot be used in infix postion" pp_token
        token
    | Invalid_prefix token ->
      Fmt.str "Syntax error: '%a' cannot be used in prefix position" pp_token
        token
    | Zero -> Fmt.str "Syntax error: empty parser result"

  let pp_error ppf = function
    | Unexpected { expected; actual } ->
      Fmt.pf ppf "@[<2>Unexpected@ {@ expected =@ @[%a@];@ actual =@ @[%a@] }@]"
        (Fmt.Dump.option pp_token) expected (Fmt.Dump.option pp_token) actual
    | Invalid_infix token ->
      Fmt.pf ppf "@[<2>Invalid_infix@ @[%a@] @]" pp_token token
    | Invalid_prefix token ->
      Fmt.pf ppf "@[<2>Invalid_prefix@ @[%a@] @]" pp_token token
    | Zero -> Fmt.pf ppf "Empty"

  type 'a parser = Lexer.t -> ('a * Lexer.t, error) result

  let return x input = Ok (x, input)

  let ( >>= ) p f input =
    match p input with
    | Ok (x, input') ->
      let p' = f x in
      p' input'
    | Error e -> Error e

  let put s _ = Ok ((), s)
  let get s = Ok (s, s)
  let zero _input = Error Zero

  let ( <|> ) p q input =
    match p input with
    | Ok value -> Ok value
    | Error _ -> q input
  (* XXX: What if p consumes input? *)
  (* | Error Empty  -> q input *)
  (* | Error e -> Error e *)

  let default x p = p <|> return x

  let rec many p =
    p >>= (fun x -> many p >>= fun xs -> return (x :: xs)) |> default []

  let combine p1 p2 =
    p1 >>= fun x ->
    p2 >>= fun y -> return (x, y)

  let some p = combine p (many p)
  let optional p = default () (p >>= fun _ -> return ())
  let error e _state = Error e

  let advance s =
    let p =
      get >>= fun lexer ->
      match Lexer.next lexer with
      | Some _token -> put lexer
      | None -> return ()
    in
    p s

  let current s =
    let p =
      get >>= fun lexer ->
      match Lexer.pick lexer with
      | Some token -> return token
      | None -> error (unexpected_end ())
    in
    p s

  let next s =
    let p =
      current >>= fun x ->
      advance >>= fun () -> return x
    in
    p s

  let expect expected : 'a parser =
    get >>= fun lexer ->
    match Lexer.pick lexer with
    | Some actual when actual = expected -> return actual
    | Some actual -> error (unexpected_token ~expected actual)
    | None -> error (unexpected_end ~expected ())

  let consume tok = expect tok >>= fun _ -> advance

  let exactly x =
    expect x >>= fun x ->
    advance >>= fun () -> return x

  let satisfy test =
    next >>= function
    | actual when test actual -> return actual
    | actual -> error (unexpected_token actual)

  let any s = (satisfy (const true)) s
  let from list = satisfy (fun x -> List.mem x list)
  let none list = satisfy (fun x -> not (List.mem x list))

  let range ?(compare = Stdlib.compare) s e =
    let ( <= ) a b = not (compare a b > 0) in
    satisfy (fun x -> s <= x && x <= e)

  let rec choice ps =
    match ps with
    | [] -> zero
    | p :: ps' -> p <|> choice ps'

  let guard = function
    | true -> return ()
    | false -> zero

  let when' test m = if test then m else return ()
  let unless test m = if test then return () else m
  let many_while test p = many (current >>= (guard << test) >>= fun () -> p)
  let some_while test p = some (current >>= (guard << test) >>= fun () -> p)

  type 'a grammar = { data : 'a scope list; term : 'a null }
  and 'a scope = { null : 'a null Table.t; left : 'a left Table.t }
  and 'a null = 'a grammar -> 'a parser
  and 'a left = ('a grammar -> 'a -> 'a parser) * int

  type 'a rule =
    | Term of 'a null
    | Null of token * 'a null
    | Left of token * 'a left

  module Grammar = struct
    type 'a t = 'a grammar

    let make_scope () = { null = Table.empty; left = Table.empty }

    let empty =
      { term = (fun g -> current >>= fun t -> error (Invalid_prefix t))
      ; data = []
      }

    let add rule grammar =
      let scope, data =
        match grammar.data with
        | [] -> (make_scope (), [])
        | scope :: grammar' -> (scope, grammar')
      in
      match rule with
      | Term term -> { grammar with term }
      | Null (t, rule) ->
        let scope = { scope with null = Table.add t rule scope.null } in
        { grammar with data = scope :: data }
      | Left (t, rule) ->
        let scope = { scope with left = Table.add t rule scope.left } in
        { grammar with data = scope :: data }

    let dump pp_token grammar =
      let dump_scope scope =
        Fmt.pr "grammar.null:\n";
        Table.iter (fun t _ -> Fmt.pr "- %a\n" pp_token t) scope.null;
        Fmt.pr "grammar.left:\n";
        Table.iter (fun t _ -> Fmt.pr "- %a\n" pp_token t) scope.left
      in
      let rec loop (data : 'a scope list) =
        match data with
        | [] -> ()
        | scope :: data' ->
          dump_scope scope;
          Fmt.pr "***@.";
          loop data'
      in
      loop grammar.data

    let get_left token grammar =
      let rec find data =
        match data with
        | [] -> None
        | scope :: data' -> begin
          match Table.get token scope.left with
          | Some rule -> Some rule
          | None -> find data'
        end
      in
      find grammar.data

    let get_null token grammar =
      let rec find data =
        match data with
        | [] -> None
        | scope :: data' -> begin
          match Table.get token scope.null with
          | Some rule -> Some rule
          | None -> find data'
        end
      in
      find grammar.data

    let has_null token grammar = is_some (get_null token grammar)
    let has_left token grammar = is_some (get_left token grammar)

    let new_scope grammar =
      { grammar with data = make_scope () :: grammar.data }

    let pop_scope grammar =
      let data =
        match grammar.data with
        | [] -> []
        | _ :: data' -> data'
      in
      { grammar with data }

    let get_term grammar = grammar.term
  end

  let nud grammar =
    current >>= fun token ->
    match Grammar.get_null token grammar with
    | Some parse -> parse grammar
    | None ->
      (* Infix tokens can only be a valid prefix if they are directly defined
         as such. If the token has a led definition it is not consumed,
         otherwise the term parser is called. *)
      if Grammar.has_left token grammar then error (invalid_prefix token)
      else
        let parse = Grammar.get_term grammar in
        parse grammar

  let rec led rbp grammar x =
    get >>= fun lexer ->
    match Lexer.pick lexer with
    | Some token -> begin
      match Grammar.get_left token grammar with
      | Some (parse, lbp) ->
        if lbp > rbp then parse grammar x >>= led rbp grammar else return x
      | None -> return x
    end
    | None -> return x

  let parse ?precedence:(rbp = 0) grammar = nud grammar >>= led rbp grammar

  let parse_many grammar =
    many
      begin
        current >>= fun token ->
        guard (not (Grammar.has_left token grammar)) >>= fun () -> nud grammar
      end

  let parse_some grammar =
    some
      begin
        current >>= fun token ->
        guard (not (Grammar.has_left token grammar)) >>= fun () -> nud grammar
      end

  let grammar rules = List.fold_left (flip Grammar.add) Grammar.empty rules

  let run (p : 'a parser) lexer =
    match p lexer with
    | Ok (x, _) -> Ok x
    | Error e -> Error e

  let rule token parse = Null (token, parse)
  let term parse = Term parse

  let infix precedence token f =
    let parse grammar x =
      advance >>= fun () ->
      parse ~precedence grammar >>= fun y -> return (f x y)
    in
    Left (token, (parse, precedence))

  let infixr precedence token f =
    let parse grammar x =
      advance >>= fun () ->
      parse ~precedence:(precedence - 1) grammar >>= fun y -> return (f x y)
    in
    Left (token, (parse, precedence))

  let prefix token f =
    let parse grammar =
      advance >>= fun () ->
      parse grammar >>= fun x -> return (f x)
    in
    Null (token, parse)

  let postfix precedence token f =
    let parse grammar x = advance >>= fun () -> return (f x) in
    Left (token, (parse, precedence))

  let between token1 token2 f =
    let parse grammar =
      advance >>= fun () ->
      parse grammar >>= fun x ->
      consume token2 >>= fun () -> return (f x)
    in
    Null (token1, parse)

  let delimiter token =
    let parse g x = error (Invalid_infix token) in
    Left (token, (parse, 0))

  let null token parse = Null (token, parse)
  let left precedence token parse = Left (token, (parse, precedence))

  let binary f g a =
    advance >>= fun () ->
    parse g >>= fun b -> return (f a b)

  let unary f g =
    advance >>= fun () ->
    parse g >>= fun a -> return (f a)
end
