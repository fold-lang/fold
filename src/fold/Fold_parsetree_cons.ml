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
  type package_type
  type row_field
  type object_field
  type pattern
  type expression
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
  type class_signature
  type class_type_field
  type 'a class_infos = 'a Parsetree.class_infos
  type class_description
  type class_type_declaration
  type class_expr
  type class_structure
  type class_field
  type class_field_kind
  type class_declaration
  type module_type
  type functor_parameter
  type signature_item
  type signature = signature_item list
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
  type structure_item
  type structure = structure_item list
  type value_binding
  type module_binding
  type toplevel_phrase
  type toplevel_directive
  type directive_argument
end

module type S = sig
  include T

  val attribute :
    loc:loc -> name:string with_loc -> payload:payload -> attribute

  val binding_op :
    loc:loc -> op:string with_loc -> pat:pattern -> exp:expression -> binding_op

  val case : lhs:pattern -> guard:expression option -> rhs:expression -> case
  val pcl_constr : loc:loc -> longident_loc -> core_type list -> class_expr
  val pcl_structure : loc:loc -> class_structure -> class_expr

  val pcl_fun :
       loc:loc
    -> Asttypes.arg_label
    -> expression option
    -> pattern
    -> class_expr
    -> class_expr

  val pcl_apply :
       loc:loc
    -> class_expr
    -> (Asttypes.arg_label * expression) list
    -> class_expr

  val pcl_let :
       loc:loc
    -> Asttypes.rec_flag
    -> value_binding list
    -> class_expr
    -> class_expr

  val pcl_constraint : loc:loc -> class_expr -> class_type -> class_expr
  val pcl_extension : loc:loc -> extension -> class_expr
  val pcl_open : loc:loc -> open_description -> class_expr -> class_expr

  val pcf_inherit :
       loc:loc
    -> Asttypes.override_flag
    -> class_expr
    -> string with_loc option
    -> class_field

  val pcf_val :
       loc:loc
    -> string with_loc * Asttypes.mutable_flag * class_field_kind
    -> class_field

  val pcf_method :
       loc:loc
    -> string with_loc * Asttypes.private_flag * class_field_kind
    -> class_field

  val pcf_constraint : loc:loc -> core_type * core_type -> class_field
  val pcf_initializer : loc:loc -> expression -> class_field
  val pcf_attribute : loc:loc -> attribute -> class_field
  val pcf_extension : loc:loc -> extension -> class_field

  val class_infos :
       loc:loc
    -> virt:Asttypes.virtual_flag
    -> params:(core_type * (Asttypes.variance * Asttypes.injectivity)) list
    -> name:string with_loc
    -> expr:'a
    -> 'a class_infos

  val class_signature :
    self:core_type -> fields:class_type_field list -> class_signature

  val class_structure :
    self:pattern -> fields:class_field list -> class_structure

  val pcty_constr : loc:loc -> longident_loc -> core_type list -> class_type
  val pcty_signature : loc:loc -> class_signature -> class_type

  val pcty_arrow :
    loc:loc -> Asttypes.arg_label -> core_type -> class_type -> class_type

  val pcty_extension : loc:loc -> extension -> class_type
  val pcty_open : loc:loc -> open_description -> class_type -> class_type
  val pctf_inherit : loc:loc -> class_type -> class_type_field

  val pctf_val :
       loc:loc
    -> string with_loc
       * Asttypes.mutable_flag
       * Asttypes.virtual_flag
       * core_type
    -> class_type_field

  val pctf_method :
       loc:loc
    -> string with_loc
       * Asttypes.private_flag
       * Asttypes.virtual_flag
       * core_type
    -> class_type_field

  val pctf_constraint : loc:loc -> core_type * core_type -> class_type_field
  val pctf_attribute : loc:loc -> attribute -> class_type_field
  val pctf_extension : loc:loc -> extension -> class_type_field
  val ptyp_any : loc:loc -> core_type
  val ptyp_var : loc:loc -> string -> core_type

  val ptyp_arrow :
    loc:loc -> Asttypes.arg_label -> core_type -> core_type -> core_type

  val ptyp_tuple : loc:loc -> core_type list -> core_type
  val ptyp_constr : loc:loc -> longident_loc -> core_type list -> core_type

  val ptyp_object :
    loc:loc -> object_field list -> Asttypes.closed_flag -> core_type

  val ptyp_class : loc:loc -> longident_loc -> core_type list -> core_type
  val ptyp_alias : loc:loc -> core_type -> string -> core_type

  val ptyp_variant :
       loc:loc
    -> row_field list
    -> Asttypes.closed_flag
    -> string list option
    -> core_type

  val ptyp_poly : loc:loc -> string with_loc list -> core_type -> core_type
  val ptyp_package : loc:loc -> package_type -> core_type
  val ptyp_extension : loc:loc -> extension -> core_type
  val pdir_string : loc:loc -> string -> directive_argument
  val pdir_int : loc:loc -> string -> char option -> directive_argument
  val pdir_ident : loc:loc -> ident -> directive_argument
  val pdir_bool : loc:loc -> bool -> directive_argument

  (** {1 Expression} *)

  module E : sig
    val ident : ?loc:loc -> longident_loc -> expression
    val constant : ?loc:loc -> constant -> expression

    val let' :
         ?loc:loc
      -> Asttypes.rec_flag
      -> value_binding list
      -> expression
      -> expression

    val function' : ?loc:loc -> case list -> expression

    val fun' :
         ?loc:loc
      -> Asttypes.arg_label
      -> expression option
      -> pattern
      -> expression
      -> expression

    val apply :
         ?loc:loc
      -> expression
      -> (Asttypes.arg_label * expression) list
      -> expression

    val match' : ?loc:loc -> expression -> case list -> expression
    val try' : ?loc:loc -> expression -> case list -> expression
    val tuple : ?loc:loc -> expression list -> expression
    val construct : ?loc:loc -> longident_loc -> expression option -> expression
    val variant : ?loc:loc -> string -> expression option -> expression

    val record :
         ?loc:loc
      -> (longident_loc * expression) list
      -> expression option
      -> expression

    val field : ?loc:loc -> expression -> longident_loc -> expression

    val setfield :
      ?loc:loc -> expression -> longident_loc -> expression -> expression

    val array : ?loc:loc -> expression list -> expression

    val ifthenelse :
      ?loc:loc -> expression -> expression -> expression option -> expression

    val sequence : ?loc:loc -> expression -> expression -> expression
    val while' : ?loc:loc -> expression -> expression -> expression

    val for' :
         ?loc:loc
      -> pattern
      -> expression
      -> expression
      -> Asttypes.direction_flag
      -> expression
      -> expression

    val constraint' : ?loc:loc -> expression -> core_type -> expression

    val coerce :
      ?loc:loc -> expression -> core_type option -> core_type -> expression

    val send : ?loc:loc -> expression -> string with_loc -> expression
    val new' : ?loc:loc -> longident_loc -> expression
    val setinstvar : ?loc:loc -> string with_loc -> expression -> expression
    val override : ?loc:loc -> (string with_loc * expression) list -> expression

    val module' :
         ?loc:loc
      -> string option with_loc
      -> module_expr
      -> expression
      -> expression

    val exception' :
      ?loc:loc -> extension_constructor -> expression -> expression

    val assert' : ?loc:loc -> expression -> expression
    val lazy' : ?loc:loc -> expression -> expression
    val poly : ?loc:loc -> expression -> core_type option -> expression
    val object' : ?loc:loc -> class_structure -> expression
    val newtype : ?loc:loc -> string with_loc -> expression -> expression
    val pack : ?loc:loc -> module_expr -> expression
    val open' : ?loc:loc -> open_declaration -> expression -> expression
    val letop : ?loc:loc -> letop -> expression
    val extension : ?loc:loc -> extension -> expression
    val unreachable : ?loc:loc -> expression
    val attr : attributes -> expression -> expression
  end

  val extension_constructor :
       loc:loc
    -> name:string with_loc
    -> kind:extension_constructor_kind
    -> extension_constructor

  val include_infos : loc:loc -> 'a -> 'a include_infos

  val label_declaration :
       loc:loc
    -> name:string with_loc
    -> mutable_:Asttypes.mutable_flag
    -> type_:core_type
    -> label_declaration

  val letop :
    let_:binding_op -> ands:binding_op list -> body:expression -> letop

  val location :
    start:Lexing.position -> end_:Lexing.position -> ghost:bool -> loc

  val module_binding :
    loc:loc -> name:string option with_loc -> expr:module_expr -> module_binding

  val module_declaration :
       loc:loc
    -> name:string option with_loc
    -> type_:module_type
    -> module_declaration

  val pmod_ident : loc:loc -> longident_loc -> module_expr
  val pmod_structure : loc:loc -> structure -> module_expr
  val pmod_functor : loc:loc -> functor_parameter -> module_expr -> module_expr
  val pmod_apply : loc:loc -> module_expr -> module_expr -> module_expr
  val pmod_constraint : loc:loc -> module_expr -> module_type -> module_expr
  val pmod_unpack : loc:loc -> expression -> module_expr
  val pmod_extension : loc:loc -> extension -> module_expr

  val module_substitution :
       loc:loc
    -> name:string with_loc
    -> manifest:longident_loc
    -> module_substitution

  val pmty_ident : loc:loc -> longident_loc -> module_type
  val pmty_signature : loc:loc -> signature -> module_type
  val pmty_functor : loc:loc -> functor_parameter -> module_type -> module_type
  val pmty_with : loc:loc -> module_type -> with_constraint list -> module_type
  val pmty_typeof : loc:loc -> module_expr -> module_type
  val pmty_extension : loc:loc -> extension -> module_type
  val pmty_alias : loc:loc -> longident_loc -> module_type

  val module_type_declaration :
       loc:loc
    -> name:string with_loc
    -> type_:module_type option
    -> module_type_declaration

  val otag : loc:loc -> string with_loc -> core_type -> object_field
  val oinherit : loc:loc -> core_type -> object_field

  val open_infos :
    loc:loc -> expr:'a -> override:Asttypes.override_flag -> 'a open_infos

  val ppat_any : loc:loc -> pattern
  val ppat_var : loc:loc -> string with_loc -> pattern
  val ppat_alias : loc:loc -> pattern -> string with_loc -> pattern
  val ppat_constant : loc:loc -> constant -> pattern
  val ppat_interval : loc:loc -> constant -> constant -> pattern
  val ppat_tuple : loc:loc -> pattern list -> pattern
  val ppat_variant : loc:loc -> string -> pattern option -> pattern

  val ppat_record :
    loc:loc -> (longident_loc * pattern) list -> Asttypes.closed_flag -> pattern

  val ppat_array : loc:loc -> pattern list -> pattern
  val ppat_or : loc:loc -> pattern -> pattern -> pattern
  val ppat_constraint : loc:loc -> pattern -> core_type -> pattern
  val ppat_type : loc:loc -> longident_loc -> pattern
  val ppat_lazy : loc:loc -> pattern -> pattern
  val ppat_unpack : loc:loc -> string option with_loc -> pattern
  val ppat_exception : loc:loc -> pattern -> pattern
  val ppat_extension : loc:loc -> extension -> pattern
  val ppat_open : loc:loc -> longident_loc -> pattern -> pattern
  val rtag : loc:loc -> string with_loc -> bool -> core_type list -> row_field
  val rinherit : loc:loc -> core_type -> row_field
  val psig_value : loc:loc -> value_description -> signature_item

  val psig_type :
    loc:loc -> Asttypes.rec_flag -> type_declaration list -> signature_item

  val psig_typesubst : loc:loc -> type_declaration list -> signature_item
  val psig_typext : loc:loc -> type_extension -> signature_item
  val psig_exception : loc:loc -> type_exception -> signature_item
  val psig_module : loc:loc -> module_declaration -> signature_item
  val psig_modsubst : loc:loc -> module_substitution -> signature_item
  val psig_recmodule : loc:loc -> module_declaration list -> signature_item
  val psig_modtype : loc:loc -> module_type_declaration -> signature_item
  val psig_modtypesubst : loc:loc -> module_type_declaration -> signature_item
  val psig_open : loc:loc -> open_description -> signature_item
  val psig_include : loc:loc -> include_description -> signature_item
  val psig_class : loc:loc -> class_description list -> signature_item
  val psig_class_type : loc:loc -> class_type_declaration list -> signature_item
  val psig_attribute : loc:loc -> attribute -> signature_item
  val psig_extension : loc:loc -> extension -> attributes -> signature_item
  val pstr_eval : loc:loc -> expression -> attributes -> structure_item

  val pstr_value :
    loc:loc -> Asttypes.rec_flag -> value_binding list -> structure_item

  val pstr_primitive : loc:loc -> value_description -> structure_item

  val pstr_type :
    loc:loc -> Asttypes.rec_flag -> type_declaration list -> structure_item

  val pstr_typext : loc:loc -> type_extension -> structure_item
  val pstr_exception : loc:loc -> type_exception -> structure_item
  val pstr_module : loc:loc -> module_binding -> structure_item
  val pstr_recmodule : loc:loc -> module_binding list -> structure_item
  val pstr_modtype : loc:loc -> module_type_declaration -> structure_item
  val pstr_open : loc:loc -> open_declaration -> structure_item
  val pstr_class : loc:loc -> class_declaration list -> structure_item
  val pstr_class_type : loc:loc -> class_type_declaration list -> structure_item
  val pstr_include : loc:loc -> include_declaration -> structure_item
  val pstr_attribute : loc:loc -> attribute -> structure_item
  val pstr_extension : loc:loc -> extension -> attributes -> structure_item

  val toplevel_directive :
       loc:loc
    -> name:string with_loc
    -> arg:directive_argument option
    -> toplevel_directive

  val type_declaration :
       loc:loc
    -> name:string with_loc
    -> params:(core_type * (Asttypes.variance * Asttypes.injectivity)) list
    -> cstrs:(core_type * core_type * loc) list
    -> kind:type_kind
    -> private_:Asttypes.private_flag
    -> manifest:core_type option
    -> type_declaration

  val type_exception : loc:loc -> extension_constructor -> type_exception

  val type_extension :
       loc:loc
    -> path:longident_loc
    -> params:(core_type * (Asttypes.variance * Asttypes.injectivity)) list
    -> constructors:extension_constructor list
    -> private_:Asttypes.private_flag
    -> type_extension

  val value_binding : loc:loc -> pat:pattern -> expr:expression -> value_binding

  val value_description :
       loc:loc
    -> name:string with_loc
    -> type_:core_type
    -> prim:string list
    -> value_description

  val ppat_construct : loc:loc -> ident with_loc -> pattern option -> pattern

  val constructor_declaration :
       loc:loc
    -> name:string with_loc
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
end
