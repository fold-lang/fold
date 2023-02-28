# Errors

Semicolon before else:
```
if b e1; else e2

unexpected "else"
```

Semicolon after else:
```
if b {
  e1 
} else
  e2;
e3

unexpected "else"
```