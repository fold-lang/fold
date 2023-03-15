# Macros

## Token Tree

- Equivalent to bare s-expressions.
- No semantics, pure structure.
- Focuses on delimiting scopes of token sequences.


## Macro call scoping

- Bracket symbols delimit scope.
- Separator symbols delimit scope.
- Should have scoping rules like function calls.

```
a + kwd! { ... } + b
==> a + (kwd! { ... }) + b

[a, kwd! b, c]
==> [a, (kwd! b), c]

2 + kwd! 2 * (3 - 4) - 1
==> 2 + (kwd! 2) * (3 - 4) - 1

[2, kwd! 2 * (3 - 4), 1, 3]
==> [2, (kwd! 2) * (3 - 4), 1, 3]      // How does this work?

[1, kwd! 2 3 4, 10]
==> [1, (kwd! 2 3 4), 10]              // How does this work?
```


## Additional features

- Quotation and anti-quotation.
- Structured forms (e.g., if-form, let-form, etc.).
- Operator parsing: prefix, infix, mixfix.
- Parsing classes: expr, type, pattern, etc.
- Extensibility: `defmacro`, etc.
- Macro call: `kwd! ...`.


## Macro call

```ocaml
ocaml! {
  let x = 2 in
  let open Printf in printf “%d” x
}

json! {
  “first_name”: “Xavier”,
  “age”: $(xavier.age),
  "items": $(json! [42, 0, 2])
}

sql! (SELECT * FROM users WHERE age > $age)

fmt! "Hello, $(name)!"
```


## Issue: scope delimiting with punctuation

Punctuation symbols that might delimit scope:
- "," - YES.
- ";" - YES.
- "=" - NO. Not necessary for fold, needed for OCaml (let a = c, d in).
- ":" - NO. Blocks are not indented, so this seems unnecessary.
- "|" - NO. Can be a normal infix symbol.
- "->" - NO. Can be a normal infix symbol.

```
a = b, c
| (a = b), c
| a = (b, c)
```

```
let a = 1, b = 2, c = 3; d

((`let` `a` `=` `1`), (`b` `=` `2`), (`c` `=` `3`)); `d`
```
