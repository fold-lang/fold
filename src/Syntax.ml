
open Local
open Lex
module String = Astring.String

module Name = struct
    type t =
      [ `ID of string
      | `Application of t * t
      | `Dot of t * string ]
    [@@deriving show, eq]

    let id x = `ID x

    let dot self str = `Dot (self, str)

    let application a b = `Application (a, b)
end


module Pattern = struct
  type t = [
    | `Tuple of t list
    | `Application of t * t list
    | token
  ] [@@deriving show, eq]

  let rec pp ppf self =
    match self with
    | #token as t ->
      Token.pp ppf t

    | `Tuple xs ->
      Fmt.pf ppf "(%a)" (Fmt.list ~sep:(Fmt.unit ", ") pp) xs

    | `Application (`Symbol x, xs) when Char.Ascii.is_letter (String.get x 0) ->
      Fmt.pf ppf "(%s %a)" x (Fmt.list ~sep:(Fmt.unit " ") pp) xs

    | `Application (`Symbol op, [a1; a2]) ->
      Fmt.pf ppf "(%a %s %a)" pp a1 op pp a2

    | `Application (f, xs) ->
      Fmt.pf ppf "(%a %a)" pp f (Fmt.list ~sep:(Fmt.unit " ") pp) xs

    (* | _ -> _pp formatter self *)


  let token x = (x :> t)

  let application f xs = `Application (f, xs)

  let tuple items = `Tuple items
end

module Expression = struct
  type t = [
    | `Let of (Pattern.t * t) list * t
    | `Application of t * t list
    | `Fn of Pattern.t * t
    | `If of t * t * t
    | `Tuple of t list
    | token
  ] [@@deriving show, eq]


  let rec pp ppf self =
    let pp_binding =
      Fmt.pair ~sep:(Fmt.unit " = ") Pattern.pp pp in

    let pp_bindings =
      Fmt.list ~sep:(Fmt.unit ",@,") pp_binding in

    match self with
    | #token as t -> Token.pp ppf t
    | `Let ([(`Symbol x, v)], body) ->
      Fmt.pf ppf "let %s =@,%a@,in@,%a]" x pp v pp body

    | `Let (bs, body) ->
      Fmt.pf ppf "@[<v2>let@,%a@]@,in@[<v>@,%a@]" pp_bindings bs pp body

    | `Application (`Symbol x, xs) when Char.Ascii.is_letter (String.get x 0) ->
      Fmt.pf ppf "(%s %a)" x (Fmt.list ~sep:(Fmt.unit " ") pp) xs

    | `Application (`Symbol op, [a1; a2]) ->
      Fmt.pf ppf "(%a %s %a)" pp a1 op pp a2

    | `Application (f, xs) ->
      Fmt.pf ppf  "<v2>(%a %a)" pp f (Fmt.list ~sep:(Fmt.unit " ") pp) xs

    | `If (a, b, c) ->
      Fmt.pf ppf "@[<v2>if %a then@,@]%a@,@[<v2>else@,%a@]" pp a pp b pp c

    | `Tuple xs ->
      Fmt.pf ppf "(%a)" (Fmt.list ~sep:(Fmt.unit ", ") pp) xs

    | _ -> failwith "pp not implemented"


  let token x = (x :> t)
  let let' bindings body = `Let (bindings, body)
  let if' a b c : t = `If (a, b, c)
  let application f xs = `Application (f, xs)
  let fn arg body = `Fn (arg, body)
  let tuple items = `Tuple items
end

module Type = struct
  type t = [
    | `Constructor of Name.t
    | `Var of string
    | `Tuple of t list
  ] [@@deriving show, eq]

  let constructor name = `Constructor name
  let var name = `Var name
  let tuple types = `Tuple types
end

module Statement = struct
  type t = [
    | `Val of Pattern.t * Expression.t
    | `Def of Pattern.t * Expression.t
    | `Type of Name.t * Name.t list * Type.t
  ] [@@deriving show, eq]

  let rec pp ppf t =
    match t with
    | `Val (n, v) -> Fmt.pf ppf "@[<hv2>val %a =@ %a@]" Pattern.pp n Expression.pp v
    | `Def (n, v) -> Fmt.pf ppf "@[<hv2>def %a =@ %a@]" Pattern.pp n Expression.pp v
    | _ -> failwith "pp not implemented"


  let val' pat expr : t = `Val (pat, expr)
  let def params expr = `Def (params, expr)
  let type' name params t = `Type (name, params, t)
end

module Module = struct
  type t = Statement.t list
  [@@deriving show, eq]

  let make xs = xs
end

