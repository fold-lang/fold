
## Semicolons instead of `in`

Pro:
- Moving bindings between local and global scope is easy.


## Using `,` instead of `|`

Pro:
- The alignment for the line after `|` looks better:
```
match x {
  Some x ->
    x + 1,
  None ->
    0
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
