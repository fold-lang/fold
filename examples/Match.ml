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
