
open Pure

module C = Colors

module type Testable = sig
  type t
  include Equatable.Base with type t := t
  include Printable.Base with type t := t
end

type 'a testable = (module Testable with type t = 'a)

let pp (type a) (t: a testable) = let (module T) = t in T.pp

let equal (type a) (t: a testable) = let (module T) = t in T.equal



let time ?fmt f x =
  let t0 = Unix.gettimeofday () in
  let fx = f x in
  let t1 = Unix.gettimeofday () -. t0 in
  let () = match fmt with
  | Some fmt -> Printf.eprintf "%s\n" (fmt fx t1)
  | None     -> Printf.eprintf "Elapsed time: %f sec\n" t1 in
  fx

let test ty msg actual expected () =
  let ok = equal ty actual expected in
  begin if not ok then begin
    print (Fmt.strf "  %s %s" (C.bright_red "✗") (C.bright_white msg));
    print (Fmt.strf "    %s %a" (C.bright_green "-") (pp ty) expected);
    print (Fmt.strf "    %s %a" (C.bright_red "+") (pp ty) actual)
  end else
    print ("  %s %s" % (C.bright_green "✓", C.bright_white msg))
  end;
  ok

let testable (type a) (pp: a Fmt.t) (equal: a -> a -> bool) : a testable =
  let module M = struct
    type t = a
    let pp = pp
    let equal = equal
  end in
  (module M)

let group name tests =
  print ("━━━ %s ━━━" % C.bright_blue name);
  let t0 = Unix.gettimeofday () in
  let s, f, t =
    List.fold_left begin fun (s, f, t) test ->
        if test () then (s + 1, f, t + 1) else (s, f + 1, t + 1)
      end
      (0, 0, 0) tests in
  let t = Unix.gettimeofday () -. t0 in
  let msg =
    match s, f with
    | 1, 0 -> "Test passed"
    | s, 0 -> "All %d tests passed" % s
    | 0, 1 -> "Test failed"
    | 0, f -> "All %d tests failed" % f
    | s, f -> "%d tests passed, %d tests failed" % (s, f) in
  print ("  %s %s in %0.2fms\n" % (C.bright_magenta "•", msg, t *. 1000.0))

let int    = testable Fmt.int (=)
let int32  = testable Fmt.int32 (=)
let int64  = testable Fmt.int64 (=)
let float  = testable Fmt.float (=)
let char   = testable Fmt.char (=)
let string = testable Fmt.string (=)
let bool   = testable Fmt.bool (=)
let unit   = testable (Fmt.unit "()") (=)


let list e =
  let rec eq l1 l2 = match (l1, l2) with
    | (x::xs, y::ys) -> equal e x y && eq xs ys
    | ([], []) -> true
    | _ -> false in
  testable (Fmt.Dump.list (pp e)) eq

let slist (type a) (a : a testable) compare =
  let l = list a in
  let eq l1 l2 = equal l (List.sort compare l1) (List.sort compare l2) in
  testable (pp l) eq

let array e =
  let eq a1 a2 =
    let (m, n) = Array.(length a1, length a2) in
    let rec go i = i = m || (equal e a1.(i) a2.(i) && go (i + 1)) in
    m = n && go 0 in
  testable (Fmt.Dump.array (pp e)) eq

let pair a b =
  let eq (a1, b1) (a2, b2) = equal a a1 a2 && equal b b1 b2 in
  testable (Fmt.Dump.pair (pp a) (pp b)) eq

let option e =
  let eq x y = match (x, y) with
    | (Some a, Some b) -> equal e a b
    | (None, None) -> true
    | _ -> false in
  testable (Fmt.Dump.option (pp e)) eq

let result a e =
  let eq x y =
    match (x, y) with
    | (Ok x, Ok y) -> equal a x y
    | (Error x, Error y) -> equal e x y
    | _ -> false in
  testable (Fmt.Dump.result ~ok:(pp a) ~error:(pp e)) eq

let of_pp pp = testable pp (=)

