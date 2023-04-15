(* Single-letter variables in this module:
    - p for parser
    - g for grammar
    - l for lexer
    - t for token


   Precedence:
   - delimiter = 1, to allow lbp(1) > rbp(0) and trigger invalid infix
*)

module Lexer = Lexer

type token = Lexer.token

let pp_token = Lexer.pp_token
let ( let* ) = Result.bind

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

type 'a grammar =
  { name : string
  ; prefix : token -> ('a grammar -> 'a parser) option
  ; infix : token -> (('a -> 'a grammar -> 'a parser) * int) option
  ; default_prefix : 'a grammar -> 'a parser
  ; default_infix : 'a -> 'a grammar -> 'a parser
  }

module Grammar = struct
  let default_prefix_err g l =
    let tok = Lexer.pick l in
    invalid_prefix ~ctx:g.name tok

  let default_infix_err _left g l =
    let tok = Lexer.pick l in
    invalid_infix ~ctx:g.name tok

  let make ?(default_prefix = default_prefix_err)
      ?(default_infix = default_infix_err) ~prefix ~infix name =
    { name; prefix; infix; default_prefix; default_infix }

  let extend ~prefix ~infix g =
    let prefix tok =
      match prefix tok with
      | None -> g.prefix tok
      | some -> some
    in
    let infix tok =
      match infix tok with
      | None -> g.infix tok
      | some -> some
    in
    { g with prefix; infix }

  let get_prefix tok g = g.prefix tok
  let get_infix tok g = g.infix tok
  let has_prefix tok g = Option.is_some (get_prefix tok g)
  let has_infix tok g = Option.is_some (get_infix tok g)
end

(* Parser combinators *)

let consume expected : 'a parser =
 fun l ->
  match Lexer.pick l with
  | Eof -> unexpected_end ~ctx:"consume" ~expected ()
  | actual when actual = expected ->
    Lexer.move l;
    Ok ()
  | actual -> unexpected_token ~ctx:"consume" ~expected actual

let rec until pred (p : 'a parser) : 'a list parser =
 fun l ->
  let t = Lexer.pick l in
  if pred t then
    let* x = p l in
    let* xs = until pred p l in
    Ok (x :: xs)
  else Ok []

let parse_prefix g : 'a parser =
 fun l ->
  let tok = Lexer.pick l in
  if Lexer.is_eof tok then unexpected_end ~ctx:g.name ()
  else
    match Grammar.get_prefix tok g with
    | None -> g.default_prefix g l
    | Some rule -> rule g l

let rec parse_infix rbp left g : 'a parser =
 fun l ->
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

let parse ?precedence:(rbp = 0) g : 'a parser =
 fun l ->
  let* left = parse_prefix g l in
  parse_infix rbp left g l

let run g l =
  match parse g l with
  | Ok x ->
    let tok = Lexer.pick l in
    if Lexer.is_eof tok then x
    else failwith (error_to_string { reason = Halted tok; ctx = "run" })
  | Error err -> failwith (error_to_string err)

(* Rules *)

let parse_infix_juxt f left g l =
  let* xs =
    until
      (fun tok -> not (Grammar.has_infix tok g || Lexer.is_eof tok))
      (parse_prefix g) l
  in
  Ok (f (left :: xs))

(* let parse_infix_juxt' ?precedence f left g l =
   let tok = Lexer.pick l in
   if not (Grammar.has_infix tok g || Lexer.is_eof tok) then
     let* right = parse ?precedence g l in
     let* xs =
       until
         (fun tok -> not (Grammar.has_infix tok g || Lexer.is_eof tok))
         (parse_prefix g) l
     in
     Ok (f (left :: xs))
   else Ok (f [ left ]) *)

let parse_prefix_juxt =
  let rule g l =
    until
      (fun tok -> not (Grammar.has_infix tok g || Lexer.is_eof tok))
      (parse_prefix g) l
  in
  rule

let infix_delimiter =
  let rule _left g l =
    let tok = Lexer.pick l in
    invalid_infix ~ctx:g.name tok
  in
  (rule, 0)

let infix_unbalanced =
  let rule _left g l =
    let tok = Lexer.pick l in
    unbalanced ~ctx:g.name tok
  in
  (rule, 0)

let infix_binary precedence tok f =
  let rule left g l =
    let* () = consume tok l in
    let* right = parse ~precedence g l in
    Ok (f left right)
  in
  (rule, precedence)

let infix_right_binary precedence tok f =
  let rule left g l =
    let* () = consume tok l in
    let* right = parse ~precedence:(precedence - 1) g l in
    Ok (f left right)
  in
  (rule, precedence)

let prefix_unary ?precedence tok f =
  let rule g l =
    let* () = consume tok l in
    let* x = parse ?precedence g l in
    Ok (f x)
  in
  rule

let const x =
  let rule _g l =
    let tok = Lexer.pick l in
    let* () = consume tok l in
    Ok x
  in
  rule

let postfix_unary precedence tok f =
  let rule left _g l =
    let* () = consume tok l in
    Ok (f left)
  in
  (rule, precedence)

let prefix_invalid =
  let rule tok g _l = invalid_prefix ~ctx:g.name tok in
  rule

let infix_invalid tok =
  let rule _left g _l = invalid_infix ~ctx:g.name tok in
  (rule, 0)

let between tok1 tok2 f =
  let prefix g l =
    let* () = consume tok1 l in
    let* x = parse g l in
    let* () = consume tok2 l in
    Ok (f x)
  in
  prefix

let prefix_scope tok1 tok2 f =
  let rule g l =
    let* () = consume tok1 l in
    if Lexer.pick l = tok2 then
      let* () = consume tok2 l in
      Ok (f None)
    else
      let* x = parse g l in
      let* () = consume tok2 l in
      Ok (f (Some x))
  in
  rule

let parse_infix_seq ~sep:(sep, precedence) f left g l =
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

let infix_seq ~sep:(sep, precedence) f =
  let rule left g l =
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
  (rule, precedence)

let prefix_seq ~sep:(sep, precedence) f g l =
  let* () = consume sep l in
  let* left = parse ~precedence g l in
  parse_infix_seq ~sep:(sep, precedence) f left g l
