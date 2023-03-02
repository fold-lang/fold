let rec pp_id out (id : id) =
  match id with
  | Lident "" -> ()
  | Lident s
    when match s.[0] with
         | 'a' .. 'z' | 'A' .. 'Z' | '_' | '\'' | '(' -> false
         | _ -> true -> Fmt.parens Fmt.string out s
  | Lident s -> Fmt.string out s
  | Ldot (y, s) -> Fmt.pf out "%a.%s" pp_id y s
  | Lapply (y, s) -> Fmt.pf out "%a(%a)" pp_id y pp_id s

let m01 =
  match name with
  | Some x ->
    print_endline x;
    "Hello, " ^ x
  | None -> "Goodye!"

let rec do_transform (continuation : unit -> 'a action) (state : 'acc)
    (f : 'acc -> 'a -> ('b, 'acc) step) : unit -> 'b action =
 fun () ->
  match continuation () with
  | Stop -> Stop
  | Continue (el, next) -> (
    match f (state, el) with
    | Done -> Stop
    | Next (yield, next_state) ->
      Continue (yield, do_transform next next_state f)
  )
