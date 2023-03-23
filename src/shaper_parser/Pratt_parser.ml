(* Single-letter variables in this module:
    - p for parser
    - g for grammar
    - l for lexer
    - t for token


   Precedence:
   - delimiter = 1, to allow lbp(1) > rbp(0) and trigger invalid infix
*)

type token = Lexer.token

let pp_token = Lexer.pp_token
let ( let* ) = Result.bind

module Token_map = struct
  include Map.Make (struct
    type t = Lexer.token

    let compare = Lexer.compare_token
  end)

  let get tbl x = try Some (find tbl x) with Not_found -> None
end

type reason =
  | Unexpected of { expected : token option; actual : token option }
  | Invalid_infix of token
  | Invalid_prefix of token
  | Unbalanced of token
  | Zero
  | Halted of token

type error = { reason : reason; ctx : string }

let unexpected_token ~ctx ?expected actual =
  Error { reason = Unexpected { expected; actual = Some actual }; ctx }

let unexpected_end ~ctx ?expected () =
  Error { reason = Unexpected { expected; actual = None }; ctx }

let invalid_prefix ~ctx t = Error { reason = Invalid_prefix t; ctx }
let invalid_infix ~ctx t = Error { reason = Invalid_infix t; ctx }
let unbalanced ~ctx t = Error { reason = Unbalanced t; ctx }

let error_to_string err =
  match err.reason with
  | Unexpected { expected = Some t1; actual = Some t2 } ->
    Fmt.str "%s: invalid syntax: expected '%a' but got '%a'" err.ctx pp_token t1
      pp_token t2
  | Unexpected { expected = Some t; actual = None } ->
    Fmt.str "%s: invalid syntax: end of file while expecting '%a'" err.ctx
      pp_token t
  | Unexpected { expected = None; actual = None } ->
    Fmt.str "%s: invalid syntax: unexpected end of file" err.ctx
  | Unexpected { expected = None; actual = Some t } ->
    Fmt.str "%s: invalid syntax: unexpected token '%a'" err.ctx pp_token t
  | Invalid_infix token ->
    Fmt.str "%s: invalid syntax: '%a' cannot be used in infix postion" err.ctx
      pp_token token
  | Invalid_prefix token ->
    Fmt.str "%s: invalid syntax: '%a' cannot be used in prefix position" err.ctx
      pp_token token
  | Unbalanced token ->
    Fmt.str "%s: invalid syntax: unbalanced '%a'" err.ctx pp_token token
  | Zero -> Fmt.str "%s: invalid syntax: empty parser result" err.ctx
  | Halted tok ->
    Fmt.str "%s: invalid syntax: parser halted at %a" err.ctx pp_token tok

(* Parser type *)

type 'a parser = Lexer.t -> ('a, error) result

(* Grammar *)

type 'a grammar =
  { name : string
  ; data : 'a scope list
  ; default_prefix : 'a prefix
  ; default_infix : 'a -> 'a grammar -> 'a parser
  }

and 'a scope = { prefix : 'a prefix Token_map.t; infix : 'a infix Token_map.t }
and 'a prefix = 'a grammar -> 'a parser
and 'a infix = ('a -> 'a grammar -> 'a parser) * int

type 'a denotation = Prefix of token * 'a prefix | Infix of token * 'a infix

(* Parser combinators *)

let consume expected g : 'a parser =
 fun l ->
  match Lexer.pick l with
  | Eof -> unexpected_end ~ctx:g.name ~expected ()
  | actual when actual = expected ->
    Lexer.move l;
    Ok ()
  | actual -> unexpected_token ~ctx:g.name ~expected actual

let rec until pred (p : 'a parser) : 'a list parser =
 fun l ->
  let t = Lexer.pick l in
  if pred t then
    let* x = p l in
    let* xs = until pred p l in
    Ok (x :: xs)
  else Ok []

module Grammar = struct
  type 'a t = 'a grammar

  let empty_scope = { infix = Token_map.empty; prefix = Token_map.empty }

  let invalid_default_infix _left g l =
    let tok = Lexer.pick l in
    invalid_infix ~ctx:(g.name ^ " (default infix)") tok

  let def_infix token pareselet self =
    let first, rest =
      match self.data with
      | [] -> (empty_scope, [])
      | first :: rest -> (first, rest)
    in
    let first' =
      { first with infix = Token_map.add token pareselet first.infix }
    in
    { self with data = first' :: rest }

  let def_prefix token pareselet self =
    let first, rest =
      match self.data with
      | [] -> (empty_scope, [])
      | first :: rest -> (first, rest)
    in
    let first' =
      { first with prefix = Token_map.add token pareselet first.prefix }
    in
    { self with data = first' :: rest }

  let def rule self =
    match rule with
    | Prefix (token, parselet) -> def_prefix token parselet self
    | Infix (token, parselet) -> def_infix token parselet self

  let get_prefix token self =
    let rec loop data =
      match data with
      | s :: rest -> (
        match Token_map.find_opt token s.prefix with
        | None -> loop rest
        | some -> some
      )
      | [] -> None
    in
    loop self.data

  let get_infix token self =
    let rec loop data =
      match data with
      | s :: rest -> (
        match Token_map.find_opt token s.infix with
        | None -> loop rest
        | some -> some
      )
      | [] -> None
    in
    loop self.data

  let has_infix grammar token = Option.is_some (get_infix token grammar)
  let push_scope scope self = { self with data = scope :: self.data }
  let new_scope self = push_scope empty_scope self

  let pop_scope self =
    match self.data with
    | [] -> self
    | _ :: rest -> { self with data = rest }

  let scope_of_list rules =
    List.fold_left
      (fun scope -> function
        | Prefix (token, parselet) ->
          { scope with prefix = Token_map.add token parselet scope.prefix }
        | Infix (token, parselet) ->
          { scope with infix = Token_map.add token parselet scope.infix }
      )
      empty_scope rules

  let make ~default_prefix ?(default_infix = invalid_default_infix) ~name rules
      =
    let scope = scope_of_list rules in
    { data = [ scope ]; default_prefix; default_infix; name }
end

let parse_prefix g l =
  let tok = Lexer.pick l in
  if Lexer.is_eof tok then unexpected_end ~ctx:g.name ()
  else
    match Grammar.get_prefix tok g with
    | None -> g.default_prefix g l
    | Some rule -> rule g l

let rec parse_infix rbp left g l =
  let tok = Lexer.pick l in
  if Lexer.is_eof tok then Ok left
  else
    match Grammar.get_infix tok g with
    | Some (rule, lbp) ->
      if lbp > rbp then
        let* left' = rule left g l in
        parse_infix rbp left' g l
      else Ok left
    | None ->
      let* left' = g.default_infix left g l in
      parse_infix rbp left' g l

let parse ?precedence:(rbp = 0) g l =
  let* left = parse_prefix g l in
  parse_infix rbp left g l

let run g l =
  match parse g l with
  | Ok x ->
    let tok = Lexer.pick l in
    if Lexer.is_eof tok then x
    else failwith (error_to_string { reason = Halted tok; ctx = g.name })
  | Error err -> failwith (error_to_string err)

let infix_juxt f left g l =
  let* xs =
    until
      (fun tok -> not (Grammar.has_infix g tok || Lexer.is_eof tok))
      (parse_prefix g) l
  in
  Ok (f (left :: xs))

let prefix_juxt g l =
  until
    (fun tok -> not (Grammar.has_infix g tok || Lexer.is_eof tok))
    (parse_prefix g) l

let delimiter tok =
  let rule _left g _ = invalid_infix ~ctx:g.name tok in
  Infix (tok, (rule, 0))

let unbalanced_rule tok =
  let rule _left g _ = unbalanced ~ctx:g.name tok in
  Infix (tok, (rule, 0))

let infix precedence tok f =
  let rule x1 g l =
    Lexer.drop tok l;
    let* x2 = parse ~precedence g l in
    Ok (f x1 x2)
  in
  Infix (tok, (rule, precedence))

let infixr precedence tok f =
  let rule x1 g l =
    Lexer.drop tok l;
    let* x2 = parse ~precedence:(precedence - 1) g l in
    Ok (f x1 x2)
  in
  Infix (tok, (rule, precedence))

let prefix ?precedence tok f =
  let rule g l =
    Lexer.drop tok l;
    let* x = parse ?precedence g l in
    Ok (f x)
  in
  Prefix (tok, rule)

let postfix precedence tok f =
  let rule x _g l =
    Lexer.drop tok l;
    Ok (f x)
  in
  Infix (tok, (rule, precedence))

let invalid_prefix_rule tok =
  let rule g _l = invalid_prefix ~ctx:g.name tok in
  Prefix (tok, rule)

let invalid_infix_rule tok =
  let rule _left g _l = invalid_infix ~ctx:g.name tok in
  Infix (tok, (rule, 0))

let between tok1 tok2 f =
  let rule g l =
    Lexer.drop tok1 l;
    let* x = parse g l in
    Lexer.drop tok2 l;
    Ok (f x)
  in
  let rule' g l =
    let g' = Grammar.def (unbalanced_rule tok2) g in
    rule g' l
  in
  Prefix (tok1, rule')

let scope tok1 tok2 f =
  let rule g l =
    Lexer.drop tok1 l;
    if Lexer.pick l = tok2 then begin
      Lexer.drop tok2 l;
      Ok (f None)
    end
    else begin
      let* x = parse g l in
      let* () = consume tok2 g l in
      Ok (f (Some x))
    end
  in
  let rule' g l =
    let g' = Grammar.def (unbalanced_rule tok2) g in
    rule g' l
  in
  Prefix (tok1, rule')

let parse_infix_seq ~sep:(sep, precedence) left g : 'a parser =
 fun l ->
  let* xs =
    until
      (fun tok -> sep = tok)
      (fun l ->
        let* () = consume sep g l in
        parse ~precedence g l
      )
      l
  in
  Ok (left :: xs)

let seq ~sep:(sep, precedence) f =
  let rule left g : 'a parser =
   fun l ->
    let* xs =
      until
        (fun tok -> sep = tok)
        (fun l ->
          let* () = consume sep g l in
          parse ~precedence g l
        )
        l
    in
    Ok (f (left :: xs))
  in
  Infix (sep, (rule, precedence))
