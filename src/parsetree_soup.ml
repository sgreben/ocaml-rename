open Stdlib_overlay
open Asttypes
open Parsetree

type parsetree_node =
    | Pcase of case
    | Pclass_declaration of class_declaration
    | Pclass_description of class_description
    | Pclass_expr of class_expr
    | Pclass_field of class_field
    | Pclass_signature of class_signature
    | Pclass_structure of class_structure
    | Pclass_type of class_type
    | Pclass_type_declaration of class_type_declaration
    | Pclass_type_field of class_type_field
    | Pconstructor_declaration of constructor_declaration
    | Pcore_type of core_type
    | Pexpression of expression
    | Pextension_constructor of extension_constructor
    | Plabel_declaration of label_declaration
    | Pmodule_binding of module_binding
    | Pmodule_declaration of module_declaration
    | Pmodule_expr of module_expr
    | Pmodule_type of module_type
    | Pmodule_type_declaration of module_type_declaration
    | Popen_description of open_description
    | Ppackage_type of package_type
    | Ppattern of pattern
    | Psignature of signature
    | Psignature_item of signature_item
    | Pstructure of structure
    | Pstructure_item of structure_item
    | Ptype_declaration of type_declaration
    | Ptype_declarations of rec_flag * type_declaration list
    | Ptype_extension of type_extension
    | Pvalue_binding of value_binding
    | Pvalue_bindings of rec_flag * value_binding list
    | Pvalue_description of value_description
    | Pwith_constraint of with_constraint

let rec location_of = function
    | Pcase case -> None
    | Pclass_declaration class_declaration -> Some class_declaration.pci_loc
    | Pclass_description class_description -> Some class_description.pci_loc
    | Pclass_expr class_expr -> Some class_expr.pcl_loc
    | Pclass_field class_field -> Some class_field.pcf_loc
    | Pclass_signature class_signature -> None
    | Pclass_structure class_structure -> None
    | Pclass_type class_type -> Some class_type.pcty_loc
    | Pclass_type_declaration class_type_declaration -> Some class_type_declaration.pci_loc
    | Pclass_type_field class_type_field -> Some class_type_field.pctf_loc
    | Pconstructor_declaration constructor_declaration -> Some constructor_declaration.pcd_loc
    | Pcore_type core_type -> Some core_type.ptyp_loc
    | Pexpression expression -> Some expression.pexp_loc
    | Pextension_constructor extension_constructor -> Some extension_constructor.pext_loc
    | Plabel_declaration label_declaration -> Some label_declaration.pld_loc
    | Pmodule_binding module_binding -> Some module_binding.pmb_loc
    | Pmodule_declaration module_declaration -> Some module_declaration.pmd_loc
    | Pmodule_expr module_expr -> Some module_expr.pmod_loc
    | Pmodule_type module_type -> Some module_type.pmty_loc
    | Pmodule_type_declaration module_type_declaration -> Some module_type_declaration.pmtd_loc
    | Popen_description open_description -> Some open_description.popen_loc
    | Ppackage_type package_type -> None
    | Ppattern pattern -> Some pattern.ppat_loc
    | Psignature _ -> None
    | Psignature_item signature_item -> Some signature_item.psig_loc
    | Pstructure _ -> None
    | Pstructure_item structure_item -> Some structure_item.pstr_loc
    | Ptype_declaration type_declaration -> Some type_declaration.ptype_loc
    | Ptype_declarations _ -> None
    | Ptype_extension type_extension -> Some type_extension.ptyext_path.loc
    | Pvalue_binding value_binding -> Some value_binding.pvb_loc
    | Pvalue_bindings _ -> None
    | Pvalue_description value_description -> Some value_description.pval_loc
    | Pwith_constraint with_constraint -> None

let printer = Pprintast.default

(* TODO: find / work around missing cases *)
let pprint =
    function
    | Pclass_expr class_expr -> (fun f -> printer # class_expr f class_expr)
    | Pclass_field class_field -> (fun f -> printer # class_field f class_field)
    | Pclass_signature class_signature -> (fun f -> printer # class_signature f class_signature)
    | Pclass_structure class_structure -> (fun f -> printer # class_structure f class_structure)
    | Pclass_type class_type -> (fun f -> printer # class_type f class_type)
    (* | Pcase case -> printer # case f case *)
    (* | Pclass_declaration class_declaration -> printer # class_declaration f class_declaration *)
    (* | Pclass_description class_description -> printer # class_description f class_description *)
    (* | Pclass_type_declaration class_type_declaration -> (fun f -> printer # class_type_declaration f class_type_declaration )*)
    (* | Pclass_type_field class_type_field -> (fun f -> printer # class_type_field f class_type_field )*)
    (* | Pconstructor_declaration constructor_declaration -> (fun f -> printer # constructor_declaration f constructor_declaration )*)
    | Pcore_type core_type -> (fun f -> printer # core_type f core_type)
    | Pexpression expression -> (fun f -> printer # expression f expression)
    | Pextension_constructor extension_constructor -> (fun f -> printer # extension_constructor f extension_constructor)
    (* | Plabel_declaration label_declaration -> (fun f -> printer # label_declaration f label_declaration )*)
    (* | Pmodule_binding module_binding -> (fun f -> printer # module_binding f module_binding )*)
    (* | Pmodule_declaration module_declaration -> (fun f -> printer # module_declaration f module_declaration )*)
    | Pmodule_expr module_expr -> (fun f -> printer # module_expr f module_expr)
    | Pmodule_type module_type -> (fun f -> printer # module_type f module_type)
    (* | Pmodule_type_declaration module_type_declaration -> (fun f -> printer # module_type_declaration f module_type_declaration )*)
    (* | Popen_description open_description -> (fun f -> printer # open_description f open_description )*)
    (* | Ppackage_type package_type -> (fun f -> printer # package_type f package_type )*)
    | Ppattern pattern -> (fun f -> printer # pattern f pattern)
    | Psignature s -> (fun f -> printer # signature f s)
    | Psignature_item signature_item -> (fun f -> printer # signature_item f signature_item)
    | Pstructure s -> (fun f -> printer # structure f s)
    | Pstructure_item structure_item -> (fun f -> printer # structure_item f structure_item)
    | Ptype_declaration type_declaration -> (fun f -> printer # type_declaration f type_declaration)
    (* | Ptype_declarations (x,y) -> (fun f -> printer # type_declarations f (x,y) )*)
    | Ptype_extension type_extension -> (fun f -> printer # type_extension f type_extension)
    (* | Pvalue_binding value_binding -> (fun f -> printer # value_binding f value_binding )*)
    (* | Pvalue_bindings (x,y) -> (fun f -> printer # value_bindings f (x,y) )*)
    | Pvalue_description value_description -> (fun f -> printer # value_description f value_description)
    (* | Pwith_constraint with_constraint -> (fun f -> printer # with_constraint f with_constraint )*)