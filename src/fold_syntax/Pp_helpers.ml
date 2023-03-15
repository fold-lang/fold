let first_is c str = str <> "" && str.[0] = c
let last_is c str = str <> "" && str.[String.length str - 1] = c
let pf = Format.fprintf

type space_formatter = (unit, Format.formatter, unit) format

let paren :
      'a.
         ?first:space_formatter
      -> ?last:space_formatter
      -> bool
      -> (Format.formatter -> 'a -> unit)
      -> Format.formatter
      -> 'a
      -> unit =
 fun ?(first = ("" : _ format6)) ?(last = ("" : _ format6)) b fu f x ->
  if b then (
    pf f "(";
    pf f first;
    fu f x;
    pf f last;
    pf f ")")
  else fu f x
