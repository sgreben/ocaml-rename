open Asttypes
open Typedtree

type typedtree_node =
    | Tcase of case
    | Tclass_declaration of class_declaration
    | Tclass_description of class_description
    | Tclass_expr of class_expr
    | Tclass_field of class_field
    | Tclass_signature of class_signature
    | Tclass_structure of class_structure
    | Tclass_type of class_type
    | Tclass_type_declaration of class_type_declaration
    | Tclass_type_field of class_type_field
    | Tconstructor_declaration of constructor_declaration
    | Tcore_type of core_type
    | Texpression of expression
    | Textension_constructor of extension_constructor
    | Tlabel_declaration of label_declaration
    | Tlabel_description of Types.label_description
    | Tmodule_binding of module_binding
    | Tmodule_declaration of module_declaration
    | Tmodule_expr of module_expr
    | Tmodule_type of module_type
    | Tmodule_type_declaration of module_type_declaration
    | Topen_description of open_description
    | Tpackage_type of package_type
    | Tpattern of pattern
    | Tsignature of signature
    | Tsignature_item of signature_item
    | Tstructure of structure
    | Tstructure_item of structure_item
    | Ttype_declaration of type_declaration
    | Ttype_declarations of type_declaration list
    | Ttype_extension of type_extension
    | Tvalue_binding of value_binding
    | Tvalue_bindings of rec_flag * value_binding list
    | Tvalue_description of value_description
    | Twith_constraint of with_constraint

let to_string = function
    | Tcase _ -> "case"
    | Tclass_declaration _ -> "class_declaration"
    | Tclass_description _ -> "class_description"
    | Tclass_expr _ -> "class_expr"
    | Tclass_field _ -> "class_field"
    | Tclass_signature _ -> "class_signature"
    | Tclass_structure _ -> "class_structure"
    | Tclass_type _ -> "class_type"
    | Tclass_type_declaration _ -> "class_type_declaration"
    | Tclass_type_field _ -> "class_type_field"
    | Tconstructor_declaration _ -> "constructor_declaration"
    | Tcore_type _ -> "core_type"
    | Texpression _ -> "expression"
    | Textension_constructor _ -> "extension_constructor"
    | Tlabel_declaration _ -> "label_declaration"
    | Tmodule_binding _ -> "module_binding"
    | Tmodule_declaration _ -> "module_declaration"
    | Tmodule_expr _ -> "module_expr"
    | Tmodule_type _ -> "module_type"
    | Tmodule_type_declaration _ -> "module_type_declaration"
    | Topen_description _ -> "open_description"
    | Tpackage_type _ -> "package_type"
    | Tpattern _ -> "pattern"
    | Tsignature _ -> "signature"
    | Tsignature_item _ -> "signature_item"
    | Tstructure _ -> "structure"
    | Tstructure_item _ -> "structure_item"
    | Ttype_declaration _ -> "type_declaration"
    | Ttype_declarations _ -> "type_declarations"
    | Ttype_extension _ -> "type_extension"
    | Tvalue_binding _ -> "value_binding"
    | Tvalue_bindings _ -> "value_bindings"
    | Tvalue_description _ -> "value_description"
    | Twith_constraint _ -> "with_constraint"
    | Tlabel_description _ -> "label_description"

let rec location_of = function
    | Tcase case -> None
    | Tclass_declaration class_declaration -> Some class_declaration.ci_loc
    | Tclass_description class_description -> Some class_description.ci_loc
    | Tclass_expr class_expr -> Some class_expr.cl_loc
    | Tclass_field class_field -> Some class_field.cf_loc
    | Tclass_signature class_signature -> None
    | Tclass_structure class_structure -> None
    | Tclass_type class_type -> Some class_type.cltyp_loc
    | Tclass_type_declaration class_type_declaration -> Some class_type_declaration.ci_loc
    | Tclass_type_field class_type_field -> Some class_type_field.ctf_loc
    | Tconstructor_declaration constructor_declaration -> Some constructor_declaration.cd_loc
    | Tcore_type core_type -> Some core_type.ctyp_loc
    | Texpression expression -> Some expression.exp_loc
    | Textension_constructor extension_constructor -> Some extension_constructor.ext_loc
    | Tlabel_declaration label_declaration -> Some label_declaration.ld_loc
    | Tmodule_binding module_binding -> Some module_binding.mb_loc
    | Tmodule_declaration module_declaration -> Some module_declaration.md_loc
    | Tmodule_expr module_expr -> Some module_expr.mod_loc
    | Tmodule_type module_type -> Some module_type.mty_loc
    | Tmodule_type_declaration module_type_declaration -> Some module_type_declaration.mtd_loc
    | Topen_description open_description -> Some open_description.open_loc
    | Tpackage_type package_type -> Some package_type.pack_txt.loc
    | Tpattern pattern -> Some pattern.pat_loc
    | Tsignature _ -> None
    | Tsignature_item signature_item -> Some signature_item.sig_loc
    | Tstructure _ -> None
    | Tstructure_item structure_item -> Some structure_item.str_loc
    | Ttype_declaration type_declaration -> Some type_declaration.typ_loc
    | Ttype_declarations _ -> None
    | Ttype_extension type_extension -> Some type_extension.tyext_txt.loc
    | Tvalue_binding value_binding -> Some value_binding.vb_loc
    | Tvalue_bindings _ -> None
    | Tvalue_description value_description -> Some value_description.val_loc
    | Twith_constraint with_constraint -> None


(* TODO: Automate the way this code was obtained from TypedtreeIter *)

let option f x =
        match x with None -> ()
    | Some e -> f e

let rec iter_structure ~enter ~leave str =
    enter (Tstructure str);
    List.iter (iter_structure_item ~enter ~leave) str.str_items;
    leave (Tstructure str)


and iter_binding ~enter ~leave vb =
    enter (Tvalue_binding vb);
    (iter_pattern ~enter ~leave) vb.vb_pat;
    (iter_expression ~enter ~leave) vb.vb_expr;
    leave (Tvalue_binding vb)

and iter_bindings ~enter ~leave rec_flag list =
    enter (Tvalue_bindings (rec_flag, list));
    List.iter (iter_binding ~enter ~leave) list;
    leave (Tvalue_bindings (rec_flag, list))

and iter_case ~enter ~leave {c_lhs; c_guard; c_rhs} =
    (iter_pattern ~enter ~leave) c_lhs;
    option (iter_expression ~enter ~leave) c_guard;
    (iter_expression ~enter ~leave) c_rhs

and iter_cases ~enter ~leave cases =
    List.iter (iter_case ~enter ~leave) cases

and iter_structure_item ~enter ~leave item =
    enter (Tstructure_item item);
    begin
        match item.str_desc with
        | Tstr_eval (exp, _attrs) -> (iter_expression ~enter ~leave) exp
        | Tstr_value (rec_flag, list) ->
                (iter_bindings ~enter ~leave) rec_flag list
        | Tstr_primitive vd -> (iter_value_description ~enter ~leave) vd
        | Tstr_type list -> (iter_type_declarations ~enter ~leave) list
        | Tstr_typext tyext -> (iter_type_extension ~enter ~leave) tyext
        | Tstr_exception ext -> (iter_extension_constructor ~enter ~leave) ext
        | Tstr_module x -> (iter_module_binding ~enter ~leave) x
        | Tstr_recmodule list -> List.iter (iter_module_binding ~enter ~leave) list
        | Tstr_modtype mtd -> (iter_module_type_declaration ~enter ~leave) mtd
        | Tstr_open _ -> ()
        | Tstr_class list ->
                List.iter (fun (ci, _, _) -> (iter_class_declaration ~enter ~leave) ci) list
        | Tstr_class_type list ->
                List.iter
                    (fun (id, _, ct) -> (iter_class_type_declaration ~enter ~leave) ct)
                    list
        | Tstr_include incl -> (iter_module_expr ~enter ~leave) incl.incl_mod
        | Tstr_attribute _ ->
                ()
    end;
    leave (Tstructure_item item)

and iter_module_binding ~enter ~leave x =
    enter (Tmodule_binding x);
    (iter_module_expr ~enter ~leave) x.mb_expr;
    leave (Tmodule_binding x)

and iter_value_description ~enter ~leave v =
    enter (Tvalue_description v);
    (iter_core_type ~enter ~leave) v.val_desc;
    leave (Tvalue_description v)

and iter_constructor_arguments ~enter ~leave l =
    List.iter (iter_core_type ~enter ~leave) l

and iter_constructor_declaration ~enter ~leave cd =
    (iter_constructor_arguments ~enter ~leave) cd.cd_args;
    option (iter_core_type ~enter ~leave) cd.cd_res;

and iter_type_parameter ~enter ~leave (ct, v) =
    (iter_core_type ~enter ~leave) ct

and iter_type_declaration ~enter ~leave decl =
    enter (Ttype_declaration decl);
    List.iter (iter_type_parameter ~enter ~leave) decl.typ_params;
    List.iter (fun (ct1, ct2, loc) ->
            (iter_core_type ~enter ~leave) ct1;
            (iter_core_type ~enter ~leave) ct2
    ) decl.typ_cstrs;
    begin match decl.typ_kind with
        | Ttype_abstract -> ()
        | Ttype_variant list ->
                List.iter (iter_constructor_declaration ~enter ~leave) list
        | Ttype_record list ->
                List.iter
                    (fun ld ->
                        (iter_label_declaration ~enter ~leave) ld
                ) list
        | Ttype_open -> ()
    end;
    option (iter_core_type ~enter ~leave) decl.typ_manifest;
    leave (Ttype_declaration decl)

and iter_label_declaration ~enter ~leave decl =
    enter (Tlabel_declaration decl);
    (iter_core_type ~enter ~leave) decl.ld_type;
    leave (Tlabel_declaration decl)

and iter_label_description ~enter ~leave decl =
    enter (Tlabel_description decl);
    leave (Tlabel_description decl)

and iter_type_declarations ~enter ~leave decls =
    enter (Ttype_declarations decls);
    List.iter (iter_type_declaration ~enter ~leave) decls;
    leave (Ttype_declarations decls)

and iter_extension_constructor ~enter ~leave ext =
    enter (Textension_constructor ext);
    begin match ext.ext_kind with
        | Text_decl(args, ret) ->
            (iter_constructor_arguments ~enter ~leave) args;
                option (iter_core_type ~enter ~leave) ret
        | Text_rebind _ -> ()
    end;
    leave (Textension_constructor ext)

and iter_type_extension ~enter ~leave tyext =
    enter (Ttype_extension tyext);
    List.iter (iter_type_parameter ~enter ~leave) tyext.tyext_params;
    List.iter (iter_extension_constructor ~enter ~leave) tyext.tyext_constructors;
    leave (Ttype_extension tyext)

and iter_pattern ~enter ~leave pat =
    enter (Tpattern pat);
    List.iter (fun (cstr, _, _attrs) -> match cstr with
                    | Tpat_type _ -> ()
                    | Tpat_unpack -> ()
                    | Tpat_constraint ct -> (iter_core_type ~enter ~leave) ct) pat.pat_extra;
    begin
        match pat.pat_desc with
        | Tpat_any -> ()
        | Tpat_var (id, _) -> ()
        | Tpat_alias (pat1, _, _) -> (iter_pattern ~enter ~leave) pat1
        | Tpat_constant cst -> ()
        | Tpat_tuple list ->
                List.iter (iter_pattern ~enter ~leave) list
        | Tpat_construct (_, _, args) ->
                List.iter (iter_pattern ~enter ~leave) args
        | Tpat_variant (label, pato, _) ->
                begin match pato with
                        None -> ()
                    | Some pat -> (iter_pattern ~enter ~leave) pat
                end
        | Tpat_record (list, closed) ->
                List.iter (fun (_, label, pat) ->
                           (iter_label_description ~enter ~leave) label;
                           (iter_pattern ~enter ~leave) pat) list
        | Tpat_array list -> List.iter (iter_pattern ~enter ~leave) list
        | Tpat_or (p1, p2, _) -> (iter_pattern ~enter ~leave) p1; (iter_pattern ~enter ~leave) p2
        | Tpat_lazy p -> (iter_pattern ~enter ~leave) p
    end;
    leave (Tpattern pat)

and iter_expression ~enter ~leave exp =
    enter (Texpression exp);
    List.iter (function (cstr, _, _attrs) ->
        match cstr with
        | Texp_constraint ct ->
                (iter_core_type ~enter ~leave) ct
        | Texp_coerce (cty1, cty2) ->
                option (iter_core_type ~enter ~leave) cty1; (iter_core_type ~enter ~leave) cty2
        | Texp_open (_, path, _, _) -> ()
        | Texp_poly cto -> option (iter_core_type ~enter ~leave) cto
        | Texp_newtype s -> ())
        exp.exp_extra;
    begin
        match exp.exp_desc with
        | Texp_ident (path, _, _) -> ()
        | Texp_constant cst -> ()
        | Texp_let (rec_flag, list, exp) ->
                (iter_bindings ~enter ~leave) rec_flag list;
                (iter_expression ~enter ~leave) exp
        | Texp_function (label, cases, _) ->
                (iter_cases ~enter ~leave) cases
        | Texp_apply (exp, list) ->
                (iter_expression ~enter ~leave) exp;
                List.iter (fun (label, expo, _) ->
                        match expo with
                            None -> ()
                        | Some exp -> (iter_expression ~enter ~leave) exp
                ) list
        | Texp_match (exp, list1, list2, _) ->
                (iter_expression ~enter ~leave) exp;
                (iter_cases ~enter ~leave) list1;
                (iter_cases ~enter ~leave) list2;
        | Texp_try (exp, list) ->
                (iter_expression ~enter ~leave) exp;
                (iter_cases ~enter ~leave) list
        | Texp_tuple list ->
                List.iter (iter_expression ~enter ~leave) list
        | Texp_construct (_, _, args) ->
                List.iter (iter_expression ~enter ~leave) args
        | Texp_variant (label, expo) ->
                begin match expo with
                        None -> ()
                    | Some exp -> (iter_expression ~enter ~leave) exp
                end
        | Texp_record (list, expo) ->
                List.iter (fun (_, label, exp) ->
                           (iter_label_description ~enter ~leave) label;
                           (iter_expression ~enter ~leave) exp) list;
                begin match expo with
                        None -> ()
                    | Some exp -> (iter_expression ~enter ~leave) exp
                end
        | Texp_field (exp, _, label) ->
                (iter_label_description ~enter ~leave) label;
                (iter_expression ~enter ~leave) exp
        | Texp_setfield (exp1, _, label, exp2) ->
                (iter_expression ~enter ~leave) exp1;
                (iter_label_description ~enter ~leave) label;
                (iter_expression ~enter ~leave) exp2
        | Texp_array list ->
                List.iter (iter_expression ~enter ~leave) list
        | Texp_ifthenelse (exp1, exp2, expo) ->
                (iter_expression ~enter ~leave) exp1;
                (iter_expression ~enter ~leave) exp2;
                begin match expo with
                        None -> ()
                    | Some exp -> (iter_expression ~enter ~leave) exp
                end
        | Texp_sequence (exp1, exp2) ->
                (iter_expression ~enter ~leave) exp1;
                (iter_expression ~enter ~leave) exp2
        | Texp_while (exp1, exp2) ->
                (iter_expression ~enter ~leave) exp1;
                (iter_expression ~enter ~leave) exp2
        | Texp_for (id, _, exp1, exp2, dir, exp3) ->
                (iter_expression ~enter ~leave) exp1;
                (iter_expression ~enter ~leave) exp2;
                (iter_expression ~enter ~leave) exp3
        | Texp_send (exp, meth, expo) ->
                (iter_expression ~enter ~leave) exp;
            begin
                match expo with
                        None -> ()
                    | Some exp -> (iter_expression ~enter ~leave) exp
            end
        | Texp_new (path, _, _) -> ()
        | Texp_instvar (_, path, _) -> ()
        | Texp_setinstvar (_, _, _, exp) ->
                (iter_expression ~enter ~leave) exp
        | Texp_override (_, list) ->
                List.iter (fun (path, _, exp) ->
                        (iter_expression ~enter ~leave) exp
                ) list
        | Texp_letmodule (id, _, mexpr, exp) ->
                (iter_module_expr ~enter ~leave) mexpr;
                (iter_expression ~enter ~leave) exp
        | Texp_assert exp -> (iter_expression ~enter ~leave) exp
        | Texp_lazy exp -> (iter_expression ~enter ~leave) exp
        | Texp_object (cl, _) ->
                (iter_class_structure ~enter ~leave) cl
        | Texp_pack (mexpr) ->
                (iter_module_expr ~enter ~leave) mexpr
    end;
    leave (Texpression exp)

and iter_package_type ~enter ~leave pack =
    enter (Tpackage_type pack);
    List.iter (fun (s, ct) -> (iter_core_type ~enter ~leave) ct) pack.pack_fields;
    leave (Tpackage_type pack)

and iter_signature ~enter ~leave sg =
    enter (Tsignature sg);
    List.iter (iter_signature_item ~enter ~leave) sg.sig_items;
    leave (Tsignature sg)

and iter_signature_item ~enter ~leave item =
    enter (Tsignature_item item);
    begin
        match item.sig_desc with
        | Tsig_value vd ->
                (iter_value_description ~enter ~leave) vd
        | Tsig_type (list) ->
                (iter_type_declarations ~enter ~leave) list
        | Tsig_exception ext ->
                (iter_extension_constructor ~enter ~leave) ext
        | Tsig_typext tyext ->
                (iter_type_extension ~enter ~leave) tyext
        | Tsig_module md ->
                (iter_module_type ~enter ~leave) md.md_type
        | Tsig_recmodule list ->
                List.iter (fun md -> (iter_module_type ~enter ~leave) md.md_type) list
        | Tsig_modtype mtd ->
                (iter_module_type_declaration ~enter ~leave) mtd
        | Tsig_open _ -> ()
        | Tsig_include incl -> (iter_module_type ~enter ~leave) incl.incl_mod
        | Tsig_class list ->
                List.iter (iter_class_description ~enter ~leave) list
        | Tsig_class_type list ->
                List.iter (iter_class_type_declaration ~enter ~leave) list
        | Tsig_attribute _ -> ()
    end;
    leave (Tsignature_item item)

and iter_module_type_declaration ~enter ~leave mtd =
    enter (Tmodule_type_declaration mtd);
    begin
        match mtd.mtd_type with
        | None -> ()
        | Some mtype -> (iter_module_type ~enter ~leave) mtype
    end;
    leave (Tmodule_type_declaration mtd)

and iter_class_declaration ~enter ~leave cd =
    enter (Tclass_declaration cd);
    List.iter (iter_type_parameter ~enter ~leave) cd.ci_params;
    (iter_class_expr ~enter ~leave) cd.ci_expr;
    leave (Tclass_declaration cd)

and iter_class_description ~enter ~leave cd =
    enter (Tclass_description cd);
    List.iter (iter_type_parameter ~enter ~leave) cd.ci_params;
    (iter_class_type ~enter ~leave) cd.ci_expr;
    leave (Tclass_description cd)

and iter_class_type_declaration ~enter ~leave cd =
    enter (Tclass_type_declaration cd);
    List.iter (iter_type_parameter ~enter ~leave) cd.ci_params;
    (iter_class_type ~enter ~leave) cd.ci_expr;
    leave (Tclass_type_declaration cd)

and iter_module_type ~enter ~leave mty =
    enter (Tmodule_type mty);
    begin
        match mty.mty_desc with
        | Tmty_ident (path, _) -> ()
        | Tmty_alias (path, _) -> ()
        | Tmty_signature sg -> (iter_signature ~enter ~leave) sg
        | Tmty_functor (id, _, mtype1, mtype2) ->
                Misc.may (iter_module_type ~enter ~leave) mtype1; (iter_module_type ~enter ~leave) mtype2
        | Tmty_with (mtype, list) ->
                (iter_module_type ~enter ~leave) mtype;
                List.iter (fun (path, _, withc) ->
                        (iter_with_constraint ~enter ~leave) withc
                ) list
        | Tmty_typeof mexpr ->
                (iter_module_expr ~enter ~leave) mexpr
    end;
    leave (Tmodule_type mty)

and iter_with_constraint ~enter ~leave cstr =
    enter (Twith_constraint cstr);
    begin
        match cstr with
        | Twith_type decl -> (iter_type_declaration ~enter ~leave) decl
        | Twith_module _ -> ()
        | Twith_typesubst decl -> (iter_type_declaration ~enter ~leave) decl
        | Twith_modsubst _ -> ()
    end;
    leave (Twith_constraint cstr)

and iter_module_expr ~enter ~leave mexpr =
    enter (Tmodule_expr mexpr);
    begin
        match mexpr.mod_desc with
        | Tmod_ident (p, _) -> ()
        | Tmod_structure st -> (iter_structure ~enter ~leave) st
        | Tmod_functor (id, _, mtype, mexpr) ->
                Misc.may (iter_module_type ~enter ~leave) mtype;
                (iter_module_expr ~enter ~leave) mexpr
        | Tmod_apply (mexp1, mexp2, _) ->
                (iter_module_expr ~enter ~leave) mexp1;
                (iter_module_expr ~enter ~leave) mexp2
        | Tmod_constraint (mexpr, _, Tmodtype_implicit, _ ) ->
                (iter_module_expr ~enter ~leave) mexpr
        | Tmod_constraint (mexpr, _, Tmodtype_explicit mtype, _) ->
                (iter_module_expr ~enter ~leave) mexpr;
                (iter_module_type ~enter ~leave) mtype
        | Tmod_unpack (exp, mty) ->
                (iter_expression ~enter ~leave) exp
(*          (iter_module_type ~enter ~leave) mty *)
    end;
    leave (Tmodule_expr mexpr)

and iter_class_expr ~enter ~leave cexpr =
    enter (Tclass_expr cexpr);
    begin
        match cexpr.cl_desc with
        | Tcl_constraint (cl, None, _, _, _ ) ->
                (iter_class_expr ~enter ~leave) cl;
        | Tcl_structure clstr -> (iter_class_structure ~enter ~leave) clstr
        | Tcl_fun (label, pat, priv, cl, partial) ->
            (iter_pattern ~enter ~leave) pat;
            List.iter (fun (id, _, exp) -> (iter_expression ~enter ~leave) exp) priv;
            (iter_class_expr ~enter ~leave) cl

        | Tcl_apply (cl, args) ->
                (iter_class_expr ~enter ~leave) cl;
                List.iter (fun (label, expo, _) ->
                        match expo with
                            None -> ()
                        | Some exp -> (iter_expression ~enter ~leave) exp
                ) args

        | Tcl_let (rec_flat, bindings, ivars, cl) ->
            (iter_bindings ~enter ~leave) rec_flat bindings;
            List.iter (fun (id, _, exp) -> (iter_expression ~enter ~leave) exp) ivars;
                (iter_class_expr ~enter ~leave) cl

        | Tcl_constraint (cl, Some clty, vals, meths, concrs) ->
                (iter_class_expr ~enter ~leave) cl;
                (iter_class_type ~enter ~leave) clty

        | Tcl_ident (_, _, tyl) ->
                List.iter (iter_core_type ~enter ~leave) tyl
    end;
    leave (Tclass_expr cexpr)

and iter_class_type ~enter ~leave ct =
    enter (Tclass_type ct);
    begin
        match ct.cltyp_desc with
        | Tcty_signature csg -> (iter_class_signature ~enter ~leave) csg
        | Tcty_constr (path, _, list) ->
                List.iter (iter_core_type ~enter ~leave) list
        | Tcty_arrow (label, ct, cl) ->
                (iter_core_type ~enter ~leave) ct;
                (iter_class_type ~enter ~leave) cl
    end;
    leave (Tclass_type ct)

and iter_class_signature ~enter ~leave cs =
    enter (Tclass_signature cs);
    (iter_core_type ~enter ~leave) cs.csig_self;
    List.iter (iter_class_type_field ~enter ~leave) cs.csig_fields;
    leave (Tclass_signature cs)


and iter_class_type_field ~enter ~leave ctf =
    enter (Tclass_type_field ctf);
    begin
        match ctf.ctf_desc with
        | Tctf_inherit ct -> (iter_class_type ~enter ~leave) ct
        | Tctf_val (s, _mut, _virt, ct) ->
                (iter_core_type ~enter ~leave) ct
        | Tctf_method (s, _priv, _virt, ct) ->
                (iter_core_type ~enter ~leave) ct
        | Tctf_constraint  (ct1, ct2) ->
                (iter_core_type ~enter ~leave) ct1;
                (iter_core_type ~enter ~leave) ct2
        | Tctf_attribute _ -> ()
    end;
    leave (Tclass_type_field ctf)

and iter_core_type ~enter ~leave ct =
    enter (Tcore_type ct);
    begin
        match ct.ctyp_desc with
        | Ttyp_any -> ()
        | Ttyp_var s -> ()
        | Ttyp_arrow (label, ct1, ct2) ->
                (iter_core_type ~enter ~leave) ct1;
                (iter_core_type ~enter ~leave) ct2
        | Ttyp_tuple list -> List.iter (iter_core_type ~enter ~leave) list
        | Ttyp_constr (path, _, list) ->
                List.iter (iter_core_type ~enter ~leave) list
        | Ttyp_object (list, o) ->
                List.iter (fun (_, _, t) -> (iter_core_type ~enter ~leave) t) list
        | Ttyp_class (path, _, list) ->
                List.iter (iter_core_type ~enter ~leave) list
        | Ttyp_alias (ct, s) ->
                (iter_core_type ~enter ~leave) ct
        | Ttyp_variant (list, bool, labels) ->
                List.iter (iter_row_field ~enter ~leave) list
        | Ttyp_poly (list, ct) -> (iter_core_type ~enter ~leave) ct
        | Ttyp_package pack -> (iter_package_type ~enter ~leave) pack
    end;
    leave (Tcore_type ct)

and iter_class_structure ~enter ~leave cs =
    enter (Tclass_structure cs);
    (iter_pattern ~enter ~leave) cs.cstr_self;
    List.iter (iter_class_field ~enter ~leave) cs.cstr_fields;
    leave (Tclass_structure cs)


and iter_row_field ~enter ~leave rf =
    match rf with
    | Ttag (label, _attrs, bool, list) ->
            List.iter (iter_core_type ~enter ~leave) list
    | Tinherit ct -> (iter_core_type ~enter ~leave) ct

and iter_class_field ~enter ~leave cf =
    enter (Tclass_field cf);
    begin
        match cf.cf_desc with
        | Tcf_inherit (ovf, cl, super, _vals, _meths) ->
            (iter_class_expr ~enter ~leave) cl
        | Tcf_constraint (cty, cty') ->
                (iter_core_type ~enter ~leave) cty;
                (iter_core_type ~enter ~leave) cty'
        | Tcf_val (lab, _, _, Tcfk_virtual cty, _) ->
                (iter_core_type ~enter ~leave) cty
        | Tcf_val (lab, _, _, Tcfk_concrete (_, exp), _) ->
                (iter_expression ~enter ~leave) exp
        | Tcf_method (lab, _, Tcfk_virtual cty) ->
                (iter_core_type ~enter ~leave) cty
        | Tcf_method (lab, _, Tcfk_concrete (_, exp)) ->
                (iter_expression ~enter ~leave) exp
        | Tcf_initializer exp ->
                (iter_expression ~enter ~leave) exp
        | Tcf_attribute _ -> ()
    end;
    leave (Tclass_field cf)