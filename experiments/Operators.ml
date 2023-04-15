let prefix_symbols = [ '!'; '?'; '~' ]

let infix_symbols =
  [ '='; '<'; '>'; '@'; '^'; '|'; '&'; '+'; '-'; '*'; '/'; '$'; '%'; '#' ]

let special_infix_strings =
  [ "asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or"; ":="; "!="; "::" ]

let fixity_of_string = function
  | "" -> `Normal
  | s when List.mem s special_infix_strings -> `Infix s
  | s when List.mem s.[0] infix_symbols -> `Infix s
  | s when List.mem s.[0] prefix_symbols -> `Prefix s
  | s when s.[0] = '.' -> `Mixfix s
  | _ -> `Normal

let is_infix = function
  | "" -> false
  | s when List.mem s special_infix_strings -> true
  | s when List.mem s.[0] infix_symbols -> true
  | _ -> false

let is_prefix = function
  | "" -> false
  | s when List.mem s.[0] prefix_symbols -> true
  | _ -> false
