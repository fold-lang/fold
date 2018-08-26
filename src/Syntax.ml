
open Local
open Lex
module String = Astring.String

let pp_keyword ppf =
  Fmt.(pf ppf "%a" (styled `Blue string))

module Name = struct
  type 'a t =
    [ `Var of string
    | `App of 'a t * 'a t
    | `Dot of 'a t * string ]
  [@@deriving show, eq]

  type lower = [ `Lower] t
  [@@deriving show, eq]

  type upper = [ `Upper] t
  [@@deriving show, eq]

  let rec pp ppf self =
    match self with
    | `Var x -> Fmt.string ppf x

    | `App (f, x) ->
      Fmt.pf ppf "%a(%a)" pp f pp x

    | `Dot (m, x) ->
      Fmt.pf ppf "%a.%s" pp m x

  let var x = `Var x

  let dot self str = `Dot (self, str)

  let app a b = `App (a, b)

  let make ?(path = []) x =
    match path with
    | [] -> `Var x
    | m0 :: ms ->
      List.append ms [x]
        |> List.fold_left (fun r x -> `Dot (r, x)) (`Var m0)

  let lower ?path x : lower = make ?path x
  let upper ?path x : upper = make ?path x
end


module Constant = struct
  type t = [
    | `Int of int
    | `Float of float
    | `Char of char
    | `String of string
    | `Bool of bool
  ] [@@deriving show, eq]

  let pp f token =
    match token with
    | `Int x    -> (Fmt.styled `Yellow Fmt.int) f x
    | `Float x  -> Fmt.float f x
    | `Char x   -> Char.dump f x
    | `String x -> (Fmt.styled `Green Astring.String.dump) f x
    | `Bool x   -> Fmt.bool f x

  let make x = (x :> t)
  let int x : t = `Int x
end


module Pattern = struct
  type t = [
    | `Tuple of t list
    | `App of t * t list
    | `Constant of Constant.t
    | `Constructor of Name.upper
    | `Var of string
  ] [@@deriving show, eq]

  let rec pp ppf self =
    match self with
    | `Constant c ->
      Constant.pp ppf c

    | `Tuple xs ->
      Fmt.pf ppf "(%a)" (Fmt.list ~sep:(Fmt.unit ", ") pp) xs

    | `App (`Var x, xs) when Char.Ascii.is_letter (String.get x 0) ->
      Fmt.pf ppf "%s %a" x (Fmt.list ~sep:(Fmt.unit " ") pp) xs

    | `App (`Var op, [a1; a2]) ->
      Fmt.pf ppf "(%a %s %a)" pp a1 op pp a2

    | `App (f, xs) ->
      Fmt.pf ppf "%a %a" pp f (Fmt.list ~sep:(Fmt.unit " ") pp) xs

    | `Constructor c ->
      Fmt.pf ppf "%a" Name.pp c

    | `Var v ->
      Fmt.pf ppf "%s" v

    (* | _ -> _pp ppf self *)


  let var x = `Var x

  let constructor x = `Constructor x

  let constant x = `Constant x

  let app f xs = `App (f, xs)

  let tuple items = `Tuple items
end


module Expr = struct
  type case = {
    pattern : Pattern.t;
    guard : t option;
    value : t;
  }

  and t = [
    | `Let of (Pattern.t * t) list * t
    | `App of t * t list
    | `Fun of case list
    | `If of t * t * t
    | `Tuple of t list
    | `Name of Name.lower
    | `Constructor of Name.upper
    | `Constant of Constant.t
    | `Field of t * Name.lower
    | `Match of t * case list
  ] [@@deriving show, eq]


  let rec pp ppf self =
    let pp_binding =
      Fmt.pair ~sep:(Fmt.unit " = ") Pattern.pp pp in

    let pp_bindings =
      Fmt.list ~sep:(Fmt.unit ",@,") pp_binding in

    match self with
    | `Constant c -> Constant.pp ppf c

    | `Constructor name -> Name.pp ppf name

    | `Let ([(b, (`Constant v))], body) ->
      Fmt.pf ppf "%a %a = %a %a@,%a" pp_keyword "let"
        Pattern.pp b Constant.pp v pp_keyword "in" pp body

    | `Let (bs, body) ->
      Fmt.pf ppf "@[<v0>@[<v2>%a@,%a@]@,%a@[<v>@,%a@]@]" pp_keyword "let"
        pp_bindings bs pp_keyword "in" pp body

    | `App (`Name (`Var x), xs) when Char.Ascii.is_letter (String.get x 0) ->
      Fmt.pf ppf "%s %a" x (Fmt.list ~sep:(Fmt.unit " ") pp) xs

    | `App (`Name (`Var ";"), [a1; a2]) ->
      Fmt.pf ppf "@[<v0>%a;@,%a@]" pp a1 pp a2

    | `App ((`Name (`Var op)), [a1; a2]) ->
      Fmt.pf ppf "(%a %s %a)" pp a1 op pp a2

    | `App (f, xs) ->
      Fmt.pf ppf  "%a %a" pp f (Fmt.list ~sep:(Fmt.unit " ") pp) xs

    | `If (a, b, c) ->
      Fmt.pf ppf "@[<v2>%a %a %a@,@]%a@,@[<v2>%a@,%a@]"
        pp_keyword "if" pp a pp_keyword "then" pp b pp_keyword "else" pp c

    | `Tuple xs ->
      Fmt.pf ppf "(%a)" (Fmt.list ~sep:(Fmt.unit ", ") pp) xs

    | `Name n -> Name.pp ppf n

    | `Field (`Name (`Var n), x) ->
      Fmt.pf ppf "%s.%a" n Name.pp x

    | `Field (r, x) ->
      Fmt.pf ppf "(%a).%a" pp r Name.pp x

    | `Match (e, cs) ->
      Fmt.pf ppf "@[<v0>%a %a %a@,%a@]" pp_keyword "match" pp e
        pp_keyword "with" (Fmt.list ~sep:(Fmt.unit "@,| ") pp_case) cs

    | `Fun [c] ->
      Fmt.pf ppf "%a %a" pp_keyword "fun" pp_case c

    | `Fun cs ->
      Fmt.pf ppf "%a @[<v-2>%a@]" pp_keyword "fun"
        (Fmt.list ~sep:(Fmt.unit "@,| ") pp_case) cs


  and pp_case ppf self =
    (* TODO: Add guard *)
    let fmt =
      match self.value with
      | `Let _ | `App (`Name (`Var ";"), _) ->
        Fmt.pf ppf "@[<v2>%a ->@,%a@]"

      | _other ->
        Fmt.pf ppf "@[<v2>%a -> %a@]"
      in
    fmt Pattern.pp self.pattern pp self.value


  let constant x = `Constant x
  let let' bindings body = `Let (bindings, body)
  let if' a b c : t = `If (a, b, c)
  let app f xs = `App (f, xs)
  let fun' cases = `Fun cases
  let tuple items = `Tuple items
  let name n = `Name n
  let constructor n = `Constructor n
  let field r n = `Field (r, n)
  let match' e cases = `Match (e, cases)

  let case pattern ?guard value = { pattern; guard; value }
end


module Type = struct
  type t = [
    | `Constructor of Name.upper
    | `Var of string
    | `Tuple of t list
  ] [@@deriving show, eq]

  let constructor name = `Constructor name
  let var name = `Var name
  let tuple types = `Tuple types
end

module Statement = struct
  type t = [
    | `Val of Pattern.t * Expr.t
    | `Def of Pattern.t * Expr.t
    | `Type of Name.lower * Name.lower list * Type.t
  ] [@@deriving show, eq]

  let pp ppf t =
    match t with
    | `Val (n, (`Constant c)) -> Fmt.pf ppf "@[<hv2>val %a =@ %a@]" Pattern.pp n Constant.pp c
    | `Val (n, v) -> Fmt.pf ppf "@[<v2>%a %a =@,%a@]" pp_keyword "val" Pattern.pp n Expr.pp v

    | `Def (n, (`Constant c)) -> Fmt.pf ppf "@[<hv2>def %a =@ %a@]" Pattern.pp n Constant.pp c
    | `Def (n, v) -> Fmt.pf ppf "@[<v2>%a %a =@,%a@]" pp_keyword "def" Pattern.pp n Expr.pp v
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

