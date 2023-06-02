# OCaml Syntax Analysis

## Problems

See:

- https://github.com/ocaml/ocaml/pull/715
- https://github.com/ocaml/ocaml/issues/11296
- https://github.com/ocaml/ocaml/issues/9034
- https://github.com/ocaml/ocaml/pull/278
- https://github.com/ocaml/ocaml/pull/716
- https://github.com/ocaml/ocaml/pull/1480
- https://github.com/ocaml/ocaml/pull/722

### Match expression

```
match a with
| Some b ->
  match b with
  | None -> ()
  | Some c -> print_endline c
| None -> ()
```


### Lambda value in record

```
# type x = { f : string -> unit; a : int; }
# {
  f = fun x -> print_endline x;
  a = 1;
};;

Error: Unbound value a
```


### If and semi colons

```
if a then b; c
if a then b else c; d
if a then b; c else d

if a then b; c ==> (if a then b); c
if a then let ... in b; c ==> if a then (let ... in b; c)

if ... then
  let () = () in  (* remove this line *)
  foo ();
  bar ();
```


### `fun` vs `function`

- Confusing for beginners.
- Inconsistent.

```
fun x -> x
function 1 -> 0 | x -> x
```

### Semicolon vs comma in list

```
[1; 2; 3]
[1, 2, 3]
```


### Constructors with multiple arguments

```
type a = A of int * int | B of (int * int)

let a = A (1, 2)
let b = B (1, 2)
```

### `let in` vs `let`

- Confusing and error-prone for beginners.
- Minor: inconsistent with `val`.

```
let x = ... in
let x = ...
```


### `;` vs `;;`

- Confusing for beginners.

```
let () =
  print_endline "a";
  print_endline "b";

let () =
  print_endline "c"
^
Error: Syntax error
```


### `(*)` ambiguity

- Error-prone.


### The `=` syntax and operator

- Confusing when used in let (see below).
- Inconsistent with type manifest `type a = b = ...`.
- Error-prone for newcomers (`==` vs `=`).

```
let x = b = 1
let x = b == 1

type t = b = int
```

## Minor issues

- The `@@` operator:
  - looks like noise;
  - same as ppx attributes.
- The `@` operator:
  - ok, but append for lists should be discouraged.
    - Use List.append
  - should be used for ppx attributes only.
- `let???` operators:
  - cryptic for newcomers.
  - no way to add identifiers for easier mixing.
  - `let* () = ... in` is too much (vs eg `do* ... in`).
  - `await f()` in ocaml `let* () = await f () in`
  - `let%lwt () = f () in ...` or `f ();%lwt ...`
- Reversed type arguments: `('a, int list) t` vs `t 'a (list int)`.
- Field path syntax: `M.x.M.y` vs `M.x.(M.y)`.
- List cons syntax: `x :: xs` vs `[x :: xs]`.
- Required () around functor arguments.
- Product syntax for types and expressions is different.
- Labels do not have `~` in types.
- Indexing operators: `.()`, `.[]`, `.{}` and `.???{}`.
- Use of `'` for generic types.
- Use of `` ` `` for polyvars.
- The `.` is too overloaded:
  - `M.x`
  - `(expr).l`
  - `M.x.M.x`
  - `type a . ...`
  - `_ -> .`
- Refactoring: `let f a b = ...` to `fun a b -> ...` is frequent and annoying.
- Refactoring: `let a = 1 in` to `let a = 1` is frequent and annoying.