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

type error =
  | Unexpected of { expected : token option; actual : token option }
  | Invalid_infix of token
  | Invalid_prefix of token
  | Zero
  | More

let unexpected_token ?expected actual =
  Unexpected { expected; actual = Some actual }

let unexpected_end ?expected () = Unexpected { expected; actual = None }
let invalid_prefix t = Invalid_prefix t
let invalid_infix t = Invalid_infix t

let error_to_string = function
  | Unexpected { expected = Some t1; actual = Some t2 } ->
    Fmt.str "invalid syntax: expected '%a' but got '%a'" pp_token t1 pp_token t2
  | Unexpected { expected = Some t; actual = None } ->
    Fmt.str "invalid syntax: end of file while expecting '%a'" pp_token t
  | Unexpected { expected = None; actual = None } ->
    Fmt.str "invalid syntax: unexpected end of file"
  | Unexpected { expected = None; actual = Some t } ->
    Fmt.str "invalid syntax: unexpected token '%a'" pp_token t
  | Invalid_infix token ->
    Fmt.str "invalid syntax: '%a' cannot be used in infix postion" pp_token
      token
  | Invalid_prefix token ->
    Fmt.str "invalid syntax: '%a' cannot be used in prefix position" pp_token
      token
  | Zero -> Fmt.str "invalid syntax: empty parser result"
  | More -> Fmt.str "invalid syntax: parser did not consume all input"

let pp_error ppf = function
  | Unexpected { expected; actual } ->
    Fmt.pf ppf "@[<2>Unexpected@ {@ expected =@ @[%a@];@ actual =@ @[%a@] }@]"
      (Fmt.Dump.option pp_token) expected (Fmt.Dump.option pp_token) actual
  | Invalid_infix token ->
    Fmt.pf ppf "@[<2>Invalid_infix@ @[%a@] @]" pp_token token
  | Invalid_prefix token ->
    Fmt.pf ppf "@[<2>Invalid_prefix@ @[%a@] @]" pp_token token
  | Zero -> Fmt.pf ppf "Empty"
  | More -> Fmt.pf ppf "More"

(* Parser type *)

type 'a parser = Lexer.t -> ('a, error) result

(* Parser combinators *)

let consume expected : 'a parser =
 fun l ->
  match Lexer.pick l with
  | Eof -> Error (unexpected_end ~expected ())
  | actual when actual = expected ->
    Lexer.move l;
    Ok ()
  | actual -> Error (unexpected_token ~expected actual)

let rec until pred (p : 'a parser) : 'a list parser =
 fun l ->
  let t = Lexer.pick l in
  if pred t then
    let* x = p l in
    let* xs = until pred p l in
    Ok (x :: xs)
  else Ok []

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

module Grammar = struct
  type 'a t = 'a grammar

  let empty_scope = { infix = Token_map.empty; prefix = Token_map.empty }

  let invalid_atom _g l =
    let tok = Lexer.pick l in
    Error (Invalid_prefix tok)

  let invalid_infix _left _g l =
    let tok = Lexer.pick l in
    Error (Invalid_infix tok)

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

  let make ~default_prefix ?(default_infix = invalid_infix) ~name rules =
    let scope = scope_of_list rules in
    { data = [ scope ]; default_prefix; default_infix; name }
end

let parse_prefix g l =
  let tok = Lexer.pick l in
  if Lexer.is_eof tok then Error (unexpected_end ())
  else
    match Grammar.get_prefix tok g with
    | None -> g.default_prefix g l
    | Some rule -> rule g l

let rec parse_infix rbp left g l =
  let tok = Lexer.pick l in
  if Lexer.is_eof tok then Ok left
  else
    match Option.(Grammar.get_infix tok g) with
    | Some (rule, lbp) ->
      if lbp > rbp then
        let* left' = rule left g l in
        parse_infix rbp left' g l
      else Ok left
    | None ->
      (* Give the opportunity to the user to continue parsing.
         By default [default_infix] fails. This can be used to implement
         juxtaposition. *)
      let* left' = g.default_infix left g l in
      parse_infix rbp left' g l

let parse ?precedence:(rbp = 0) g l =
  let* left = parse_prefix g l in
  parse_infix rbp left g l

let run g l =
  match parse g l with
  | Ok x ->
    if Lexer.is_eof (Lexer.pick l) then x else failwith (error_to_string More)
  | Error err -> failwith (error_to_string err)

let juxt f left g l =
  let* xs =
    until
      (fun tok -> not (Grammar.has_infix g tok || Lexer.is_eof tok))
      (parse_prefix g) l
  in
  Ok (f (left :: xs))

let delimiter tok =
  let rule _left _g _ = Error (Invalid_infix tok) in
  Infix (tok, (rule, 0))

let infix precedence tok f =
  let rule x1 g l =
    Lexer.move l;
    let* x2 = parse ~precedence g l in
    Ok (f x1 x2)
  in
  Infix (tok, (rule, precedence))

let infixr precedence tok f =
  let rule x1 g l =
    Lexer.move l;
    let* x2 = parse ~precedence:(precedence - 1) g l in
    Ok (f x1 x2)
  in
  Infix (tok, (rule, precedence))

let prefix tok f =
  let rule g l =
    Lexer.move l;
    let* x = parse g l in
    Ok (f x)
  in
  Prefix (tok, rule)

let postfix precedence tok f =
  let rule x g l =
    Lexer.move l;
    Ok (f x)
  in
  Infix (tok, (rule, precedence))

let invalid_prefix_token tok =
  let rule _g _l = Error (invalid_prefix tok) in
  Prefix (tok, rule)

let between tok1 tok2 f =
  let rule g l =
    Lexer.move l;
    let* x = parse g l in
    Lexer.drop tok2 l;
    Ok (f x)
  in
  let rule' g l =
    let g' = Grammar.def (delimiter tok2) g in
    rule g' l
  in
  Prefix (tok1, rule')

let scope tok1 tok2 f =
  let rule g l =
    Lexer.move l;
    if Lexer.pick l = tok2 then begin
      Lexer.move l;
      Ok (f None)
    end
    else begin
      let* x = parse g l in
      let* () = consume tok2 l in
      Ok (f (Some x))
    end
  in
  let rule' g l =
    let g' = Grammar.def (delimiter tok2) g in
    rule g' l
  in
  Prefix (tok1, rule')

let seq ~sep:(sep, precedence) f =
  let rule left g : 'a parser =
   fun l ->
    let* xs =
      until
        (fun tok -> sep = tok)
        (fun l ->
          let* () = consume sep l in
          parse ~precedence g l
        )
        l
    in
    Ok (f (left :: xs))
  in
  Infix (sep, (rule, precedence))
