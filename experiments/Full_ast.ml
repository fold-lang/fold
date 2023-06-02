open Prelude

type longident_loc = ident with_loc

module type T = sig
  type constant = Ml.constant
  type location_stack
  type attribute
  type attributes = attribute list
  type payload
  type extension = string with_loc * payload
  type core_type
  type core_type_desc
  type package_type
  type row_field
  type row_field_desc
  type object_field
  type object_field_desc
  type pattern
  type pattern_desc
  type expression
  type expression_desc
  type case
  type letop
  type binding_op
  type value_description
  type type_declaration
  type type_kind = Parsetree.type_kind
  type label_declaration
  type constructor_declaration
  type constructor_arguments
  type type_extension
  type extension_constructor
  type type_exception
  type extension_constructor_kind
  type class_type
  type class_type_desc
  type class_signature
  type class_type_field
  type class_type_field_desc
  type 'a class_infos = 'a Parsetree.class_infos
  type class_description
  type class_type_declaration
  type class_expr
  type class_expr_desc
  type class_structure
  type class_field
  type class_field_desc
  type class_field_kind
  type class_declaration
  type module_type
  type module_type_desc
  type functor_parameter
  type signature_item
  type signature = signature_item list
  type signature_item_desc
  type module_declaration
  type module_substitution
  type module_type_declaration
  type 'a open_infos = 'a Parsetree.open_infos
  type open_description
  type 'a include_infos = 'a Parsetree.include_infos
  type include_description
  type include_declaration
  type with_constraint
  type module_expr
  type open_declaration = module_expr open_infos
  type module_expr_desc
  type structure_item
  type structure = structure_item list
  type structure_item_desc
  type value_binding
  type module_binding
  type toplevel_phrase
  type toplevel_directive
  type directive_argument
  type directive_argument_desc
end

module type S = sig
  include T

  val attribute : name:string with_loc -> payload:payload -> attribute

  val binding_op :
    op:string with_loc -> pat:pattern -> exp:expression -> binding_op

  val case : lhs:pattern -> guard:expression option -> rhs:expression -> case
  val pcl_constr : longident_loc -> core_type list -> class_expr
  val pcl_structure : class_structure -> class_expr

  val pcl_fun :
    arg_label -> expression option -> pattern -> class_expr -> class_expr

  val pcl_apply : class_expr -> (arg_label * expression) list -> class_expr
  val pcl_let : rec_flag -> value_binding list -> class_expr -> class_expr
  val pcl_constraint : class_expr -> class_type -> class_expr
  val pcl_extension : extension -> class_expr
  val pcl_open : open_description -> class_expr -> class_expr

  val pcf_inherit :
    override_flag -> class_expr -> string with_loc option -> class_field

  val pcf_val : string with_loc * mutable_flag * class_field_kind -> class_field

  val pcf_method :
    string with_loc * private_flag * class_field_kind -> class_field

  val pcf_constraint : core_type * core_type -> class_field
  val pcf_initializer : expression -> class_field
  val pcf_attribute : attribute -> class_field
  val pcf_extension : extension -> class_field

  val class_infos :
       virt:virtual_flag
    -> params:(core_type * (variance * injectivity)) list
    -> name:string with_loc
    -> expr:'a
    -> 'a class_infos

  val class_signature :
    self:core_type -> fields:class_type_field list -> class_signature

  val class_structure :
    self:pattern -> fields:class_field list -> class_structure

  val pcty_constr : longident_loc -> core_type list -> class_type
  val pcty_signature : class_signature -> class_type
  val pcty_arrow : arg_label -> core_type -> class_type -> class_type
  val pcty_extension : extension -> class_type
  val pcty_open : open_description -> class_type -> class_type
  val pctf_inherit : class_type -> class_type_field

  val pctf_val :
       string with_loc * mutable_flag * virtual_flag * core_type
    -> class_type_field

  val pctf_method :
       string with_loc * private_flag * virtual_flag * core_type
    -> class_type_field

  val pctf_constraint : core_type * core_type -> class_type_field
  val pctf_attribute : attribute -> class_type_field
  val pctf_extension : extension -> class_type_field
  val ptyp_any : core_type
  val ptyp_var : string -> core_type
  val ptyp_arrow : arg_label -> core_type -> core_type -> core_type
  val ptyp_tuple : core_type list -> core_type
  val ptyp_constr : longident_loc -> core_type list -> core_type
  val ptyp_object : object_field list -> closed_flag -> core_type
  val ptyp_class : longident_loc -> core_type list -> core_type
  val ptyp_alias : core_type -> string -> core_type

  val ptyp_variant :
    row_field list -> closed_flag -> string list option -> core_type

  val ptyp_poly : string with_loc list -> core_type -> core_type
  val ptyp_package : package_type -> core_type
  val ptyp_extension : extension -> core_type
  val pdir_string : string -> directive_argument
  val pdir_int : string -> char option -> directive_argument
  val pdir_ident : ident -> directive_argument
  val pdir_bool : bool -> directive_argument

  (* --- pexp --- *)

  val pexp_ident : longident_loc -> expression
  val pexp_constant : constant -> expression
  val pexp_let : rec_flag -> value_binding list -> expression -> expression
  (* (let-rec [a b] (+ a b)) *)

  (* let%ok *)

  val pexp_function : case list -> expression
  (* (fn []) *)

  val pexp_fun :
    arg_label -> expression option -> pattern -> expression -> expression
  (*
    (fn [a] (+ a 1))
    (fn [a b] (+ a b))
    (fn [:a b] (+ a b))
    (fn [(:a 42) b] (+ a b))
  *)

  val pexp_apply : expression -> (arg_label * expression) list -> expression
  val pexp_match : expression -> case list -> expression
  val pexp_try : expression -> case list -> expression
  val pexp_tuple : expression list -> expression
  val pexp_construct : longident_loc -> expression option -> expression
  val pexp_variant : string -> expression option -> expression

  val pexp_record :
    (longident_loc * expression) list -> expression option -> expression

  val pexp_field : expression -> longident_loc -> expression
  val pexp_setfield : expression -> longident_loc -> expression -> expression
  val pexp_array : expression list -> expression

  val pexp_ifthenelse :
    expression -> expression -> expression option -> expression

  val pexp_sequence : expression -> expression -> expression
  val pexp_while : expression -> expression -> expression

  val pexp_for :
       pattern
    -> expression
    -> expression
    -> direction_flag
    -> expression
    -> expression

  val pexp_constraint : expression -> core_type -> expression
  val pexp_coerce : expression -> core_type option -> core_type -> expression
  val pexp_send : expression -> string with_loc -> expression
  val pexp_new : longident_loc -> expression
  val pexp_setinstvar : string with_loc -> expression -> expression
  val pexp_override : (string with_loc * expression) list -> expression

  val pexp_letmodule :
    string option with_loc -> module_expr -> expression -> expression

  val pexp_letexception : extension_constructor -> expression -> expression
  val pexp_assert : expression -> expression
  val pexp_lazy : expression -> expression
  val pexp_poly : expression -> core_type option -> expression
  val pexp_object : class_structure -> expression
  val pexp_newtype : string with_loc -> expression -> expression
  val pexp_pack : module_expr -> expression
  val pexp_open : open_declaration -> expression -> expression
  val pexp_letop : letop -> expression
  val pexp_extension : extension -> expression
  val pexp_unreachable : expression

  val extension_constructor :
       name:string with_loc
    -> kind:extension_constructor_kind
    -> extension_constructor

  val include_infos : 'a -> 'a include_infos

  val label_declaration :
       name:string with_loc
    -> mutable_:mutable_flag
    -> type_:core_type
    -> label_declaration

  val letop :
    let_:binding_op -> ands:binding_op list -> body:expression -> letop

  val location :
    start:Lexing.position -> end_:Lexing.position -> ghost:bool -> loc

  val module_binding :
    name:string option with_loc -> expr:module_expr -> module_binding

  val module_declaration :
    name:string option with_loc -> type_:module_type -> module_declaration

  val pmod_ident : longident_loc -> module_expr
  val pmod_structure : structure -> module_expr
  val pmod_functor : functor_parameter -> module_expr -> module_expr
  val pmod_apply : module_expr -> module_expr -> module_expr
  val pmod_constraint : module_expr -> module_type -> module_expr
  val pmod_unpack : expression -> module_expr
  val pmod_extension : extension -> module_expr

  val module_substitution :
    name:string with_loc -> manifest:longident_loc -> module_substitution

  val pmty_ident : longident_loc -> module_type
  val pmty_signature : signature -> module_type
  val pmty_functor : functor_parameter -> module_type -> module_type
  val pmty_with : module_type -> with_constraint list -> module_type
  val pmty_typeof : module_expr -> module_type
  val pmty_extension : extension -> module_type
  val pmty_alias : longident_loc -> module_type

  val module_type_declaration :
    name:string with_loc -> type_:module_type option -> module_type_declaration

  val otag : string with_loc -> core_type -> object_field
  val oinherit : core_type -> object_field
  val open_infos : expr:'a -> override:override_flag -> 'a open_infos
  val ppat_any : pattern
  val ppat_var : string with_loc -> pattern
  val ppat_alias : pattern -> string with_loc -> pattern
  val ppat_constant : constant -> pattern
  val ppat_interval : constant -> constant -> pattern
  val ppat_tuple : pattern list -> pattern
  val ppat_variant : string -> pattern option -> pattern
  val ppat_record : (longident_loc * pattern) list -> closed_flag -> pattern
  val ppat_array : pattern list -> pattern
  val ppat_or : pattern -> pattern -> pattern
  val ppat_constraint : pattern -> core_type -> pattern
  val ppat_type : longident_loc -> pattern
  val ppat_lazy : pattern -> pattern
  val ppat_unpack : string option with_loc -> pattern
  val ppat_exception : pattern -> pattern
  val ppat_extension : extension -> pattern
  val ppat_open : longident_loc -> pattern -> pattern
  val rtag : string with_loc -> bool -> core_type list -> row_field
  val rinherit : core_type -> row_field
  val psig_value : value_description -> signature_item
  val psig_type : rec_flag -> type_declaration list -> signature_item
  val psig_typesubst : type_declaration list -> signature_item
  val psig_typext : type_extension -> signature_item
  val psig_exception : type_exception -> signature_item
  val psig_module : module_declaration -> signature_item
  val psig_modsubst : module_substitution -> signature_item
  val psig_recmodule : module_declaration list -> signature_item
  val psig_modtype : module_type_declaration -> signature_item
  val psig_modtypesubst : module_type_declaration -> signature_item
  val psig_open : open_description -> signature_item
  val psig_include : include_description -> signature_item
  val psig_class : class_description list -> signature_item
  val psig_class_type : class_type_declaration list -> signature_item
  val psig_attribute : attribute -> signature_item
  val psig_extension : extension -> attributes -> signature_item
  val pstr_eval : expression -> attributes -> structure_item
  val pstr_value : rec_flag -> value_binding list -> structure_item
  val pstr_primitive : value_description -> structure_item
  val pstr_type : rec_flag -> type_declaration list -> structure_item
  val pstr_typext : type_extension -> structure_item
  val pstr_exception : type_exception -> structure_item
  val pstr_module : module_binding -> structure_item
  val pstr_recmodule : module_binding list -> structure_item
  val pstr_modtype : module_type_declaration -> structure_item
  val pstr_open : open_declaration -> structure_item
  val pstr_class : class_declaration list -> structure_item
  val pstr_class_type : class_type_declaration list -> structure_item
  val pstr_include : include_declaration -> structure_item
  val pstr_attribute : attribute -> structure_item
  val pstr_extension : extension -> attributes -> structure_item

  val toplevel_directive :
    name:string with_loc -> arg:directive_argument option -> toplevel_directive

  val type_declaration :
       name:string with_loc
    -> params:(core_type * (variance * injectivity)) list
    -> cstrs:(core_type * core_type * loc) list
    -> kind:type_kind
    -> private_:private_flag
    -> manifest:core_type option
    -> type_declaration

  val type_exception : extension_constructor -> type_exception

  val type_extension :
       path:longident_loc
    -> params:(core_type * (variance * injectivity)) list
    -> constructors:extension_constructor list
    -> private_:private_flag
    -> type_extension

  val value_binding : pat:pattern -> expr:expression -> value_binding

  val value_description :
       name:string with_loc
    -> type_:core_type
    -> prim:string list
    -> value_description

  val ppat_construct : ident with_loc -> pattern option -> pattern

  val constructor_declaration :
       name:string with_loc
    -> args:constructor_arguments
    -> res:core_type option
    -> constructor_declaration

  (* Extra: constructor_arguments *)
  val pcstr_tuple : core_type list -> constructor_arguments
  val pcstr_record : label_declaration list -> constructor_arguments

  (* Extra: payload *)
  val pstr : structure -> payload
  val psig : signature -> payload
  val ptyp : core_type -> payload
  val ppat : pattern -> guard:expression option -> payload

  (* Extra: type_kind *)
  val ptype_abstract : type_kind
  val ptype_variant : constructor_declaration list -> type_kind
  val ptype_record : label_declaration list -> type_kind
  val ptype_open : type_kind

  (* Extra: attributes *)
  val pexp_with_attributes : attributes -> expression -> expression
end
