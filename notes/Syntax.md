# Syntax


## Forms

Prefix interleave:
```
if _ _ else _
match _ with _
while _ do _
for _ to _ do _
for _ downto _ do _
fn _ -> _
object _ { _ }
```

Multi prefix:
```
module type _
let rec _
module rec _
class type _
module type of _
type nonrec _
inherit _ as _
```

Single prefix:
```
object _
let _
open _
include _
type _
exception _
external _
val _
initializer _
inherit _
```

Multi suffix:
```
_ with type _ := _
_ with module type _ = _
```

Scope:
```
( _ )
{ _ }
[ _ ]
```

Sep:
```
_; _; _
_, _, _
_. _. _
```


## OCaml Parsetree types

```
type 'a class_infos = 'a Parsetree.class_infos
type 'a include_infos = 'a Parsetree.include_infos
type 'a open_infos = 'a Parsetree.open_infos
type attribute = Parsetree.attribute
type attributes = Parsetree.attributes
type binding_op = Parsetree.binding_op
type case = Parsetree.case
type class_declaration = Parsetree.class_declaration
type class_description = Parsetree.class_description
type class_expr = Parsetree.class_expr
type class_field = Parsetree.class_field
type class_field_kind = Parsetree.class_field_kind
type class_signature = Parsetree.class_signature
type class_structure = Parsetree.class_structure
type class_type = Parsetree.class_type
type class_type_declaration = Parsetree.class_type_declaration
type class_type_field = Parsetree.class_type_field
type constant = Parsetree.constant
type constructor_arguments = Parsetree.constructor_arguments
type constructor_declaration = Parsetree.constructor_declaration
type core_type = Parsetree.core_type
type directive_argument = Parsetree.directive_argument
type expression = Parsetree.expression
type extension = Parsetree.extension
type extension_constructor = Parsetree.extension_constructor
type extension_constructor_kind = Parsetree.extension_constructor_kind
type functor_parameter = Parsetree.functor_parameter
type include_declaration = Parsetree.include_declaration
type include_description = Parsetree.include_description
type label_declaration = Parsetree.label_declaration
type letop = Parsetree.letop
type location_stack = Parsetree.location_stack
type module_binding = Parsetree.module_binding
type module_declaration = Parsetree.module_declaration
type module_expr = Parsetree.module_expr
type module_substitution = Parsetree.module_substitution
type module_type = Parsetree.module_type
type module_type_declaration = Parsetree.module_type_declaration
type object_field = Parsetree.object_field
type open_declaration = Parsetree.open_declaration
type open_description = Parsetree.open_description
type package_type = Parsetree.package_type
type pattern = Parsetree.pattern
type payload = Parsetree.payload
type row_field = Parsetree.row_field
type signature = Parsetree.signature
type signature_item = Parsetree.signature_item
type structure = Parsetree.structure
type structure_item = Parsetree.structure_item
type toplevel_directive = Parsetree.toplevel_directive
type toplevel_phrase = Parsetree.toplevel_phrase
type type_declaration = Parsetree.type_declaration
type type_exception = Parsetree.type_exception
type type_extension = Parsetree.type_extension
type type_kind = Parsetree.type_kind
type value_binding = Parsetree.value_binding
type value_description = Parsetree.value_description
type with_constraint = Parsetree.with_constraint
```

