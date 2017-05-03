
open Pure

type 'a result =
  | Passed
  | Failed of { actual : 'a; expected : 'a }
  | Raised of exn


let red     = id
let green   = id
let white   = id


let test desc run expected show =
  let actual = run () in
  if actual = expected then
    print ("%s %s" % (green "✓", white desc))
  else begin
    print ("%s %s" % (green "✗", white desc));
    print ("  - Expected: %s" % green (show expected));
    print ("  - Actual:   %s" % red (show actual))
  end


