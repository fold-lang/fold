type t =
  | Milliseconds
  | Second
  | Minute
  | Hour
  | Day
  | Week
  | Month
  | Quarter
  | Year
  | Decade
  | Century
  | Millennium
  | Seconds of int
  | Minutes of int
  | Hours of int

let re = Re.Posix.compile_pat "^([0-9]+)([a-z]+)$"

let parse_s_m_h str =
  let regex_str_list = List.map Re.Group.all (Re.all re str) in
  match regex_str_list with
  | [ [| _; number; "s" | "seconds" |] ] -> Seconds (int_of_string number)
  | [ [| _; number; "m" | "minutes" |] ] -> Minutes (int_of_string number)
  | [ [| _; number; "h" | "hours" |] ] -> Seconds (int_of_string number)
  | [ [| _; _number; unit |] ] ->
    Fmt.failwith "incorrect resolution unit: %s" unit
  | _ -> Fmt.failwith "invalid resolution: %s" str

let parse_constant str =
  match str with
  | "milliseconds" -> Milliseconds
  | "second" -> Second
  | "minute" -> Minute
  | "hour" -> Hour
  | "day" -> Day
  | "week" -> Week
  | "month" -> Month
  | "quarter" -> Quarter
  | "year" -> Year
  | "decade" -> Decade
  | "century" -> Century
  | "millennium" -> Millennium
  | _ -> invalid_arg str

let of_string str =
  match str with
  | "" -> invalid_arg "empty resolution"
  | _ -> (
    try parse_constant str with Invalid_argument _ -> parse_s_m_h str
  )

let to_string t =
  match t with
  | Milliseconds -> "milliseconds"
  | Second -> "second"
  | Minute -> "minute"
  | Hour -> "hour"
  | Day -> "day"
  | Week -> "week"
  | Month -> "month"
  | Quarter -> "quarter"
  | Year -> "year"
  | Decade -> "decade"
  | Century -> "century"
  | Millennium -> "millennium"
  | Seconds n -> string_of_int n ^ "seconds"
  | Minutes n -> string_of_int n ^ "minutes"
  | Hours n -> string_of_int n ^ "hours"

let is_constant t =
  match t with
  | Milliseconds
  | Second
  | Minute
  | Hour
  | Day
  | Week
  | Month
  | Quarter
  | Year
  | Decade
  | Century
  | Millennium -> true
  | Seconds _ | Minutes _ | Hours _ -> false
