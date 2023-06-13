
## Semicolons instead of `in`

Pro:
- Moving bindings between local and global scope is easy.
- Local definitions of types, exceptions, modules uses the same syntax.
- Easy navigation in vim with `f;`.


## Using `,` instead of `|`

Pro:
- The alignment for the line after `|` looks better:
- Used in Haskell, Elm.
```
match x {
  Some 0 -> {
    print "hello";
    1
  },
  Some x ->
    x + 1,
  None -> 0
}
```


## Requires scope for `if`

```
if a then b else c + 1

==> 
(if a then b else c) + 1
if a then b else (c + 1)
```

```
if a then b else c; 1

==> 
(if a then b else c) ; 1
if a then b else (c ; 1)
```


## Precedence rules for `fn`

```
let f = fn a -> a + 1;
let f = fn a -> { a } + 1;
let f = fn { a -> a } + 1;
```


## Swap order of `as` in patterns

```
xs as Some [1, 2]
```

- Easier to see the variable name.
- Consistent with label renaming `(xs as Some [1, 2]) -> ...`.
