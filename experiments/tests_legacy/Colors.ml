

let fmt = Printf.sprintf

let color_format color =
  fmt "\027[%dm%s\027[0m"
     (match color with
      | `Black   -> 30
      | `Red     -> 31
      | `Green   -> 32
      | `Yellow  -> 33
      | `Blue    -> 34
      | `Magenta -> 35
      | `Cyan    -> 36
      | `White   -> 37)

let blue               = color_format `Blue
let red                = color_format `Red
let yellow             = color_format `Yellow
let magenta            = color_format `Magenta
let cyan               = color_format `Cyan
let white              = color_format `White
let green              = color_format `Green
let bright_white x     = fmt "\027[1;37m%s\027[0m" x
let bright_blue x      = fmt "\027[1;34m%s\027[0m" x
let bright_magenta x   = fmt "\027[1;35m%s\027[0m" x
let violet x           = fmt "\027[0;34m%s\027[0m" x
let bright_red x       = fmt "\027[1;31m%s\027[0m" x
let bright_green x     = fmt "\027[1;32m%s\027[0m" x
let start_bright_white = fmt "\027[1;37m"
let start_white        = fmt "\027[37m"
let end_color          = "\027[0m"
let italic x           = "\027[3m" ^ x ^ "\027[0m"
let underline x        = "\027[4m" ^ x ^ "\027[0m"
let blink x            = "\027[5m" ^ x ^ "\027[0m"

