open Stdlib_overlay
open Compilerlibs_overlay
open Parsetree
open Parsetree_soup
open Typedtree
open Typedtree_soup
open Location
open Symbol_reference

exception Not_implemented

type op =
    | Rr_ast_node of parsetree_node
    | Rr_string of parsetree_node * string loc
    | Rr_longident of parsetree_node * Longident.t loc

let rename_value_reference rename_to sr =
    match sr.sr_fragment with
    | Af_typed (Tpattern ({pat_desc=Tpat_alias (tpat',_,sloc); _} as tpat)) ->
        let ppat = Untypeast.pattern tpat in
        let ppat' = Untypeast.pattern tpat' in
        let sloc' = {sloc with txt = rename_to} in
        Rr_string (Ppattern {ppat with ppat_desc = Ppat_alias (ppat', sloc')},
                   sloc')
    | Af_typed (Tpattern ({pat_desc=Tpat_var (_, sloc); _} as tpat)) ->
        let ppat = Untypeast.pattern tpat in
        let sloc' = {sloc with txt = rename_to} in
        Rr_string (Ppattern {ppat with ppat_desc = Ppat_var sloc'},
                   sloc')
    | Af_typed (Texpression ({exp_desc=Texp_ident (_, lloc, _); _} as texp)) ->
        let pexp = Untypeast.expression texp in
        let lloc' = {lloc with txt = Longident.rename rename_to lloc.txt} in
        Rr_longident (Pexpression {pexp with pexp_desc = Pexp_ident lloc'},
                      lloc')
    | Af_typed (Tsignature_item ({sig_desc = Tsig_value tvd;  _} as tsig)) ->
        let psig = Untypeast.signature_item tsig in
        let pvd = Untypeast.value_description tvd in
        let sloc' = {tvd.val_name with txt = rename_to } in
        let pvd = {pvd with pval_name = sloc'} in
        Rr_string (Psignature_item {psig with psig_desc = Psig_value pvd},
                   sloc')
    | _ -> raise Not_implemented

let rename_type_reference rename_to sr =
    match sr.sr_fragment with
    | Af_typed (Ttype_declaration ({typ_name; _} as td)) ->
        let ptd = Untypeast.type_declaration td in
        let sloc' = {typ_name with txt = rename_to} in
        Rr_string (Ptype_declaration {ptd with ptype_name = sloc'},
                   sloc')
    | Af_typed (Tcore_type ({ctyp_desc=Ttyp_constr (_, lloc, cts); _} as ct)) ->
        let pct = Untypeast.core_type ct in
        let pcts = List.map Untypeast.core_type cts in
        let lloc' = {lloc with txt = Longident.rename rename_to lloc.txt} in
        Rr_longident (Pcore_type {pct with ptyp_desc = Ptyp_constr (lloc', pcts)},
                      lloc')
    | _ -> raise Not_implemented

let rename_field_reference rename_to sr =
    match sr.sr_fragment with
    | Af_typed (Texpression ({exp_desc=Texp_field (exp2, lloc, _); _} as exp)) ->
        let pexp= Untypeast.expression exp in
        let pexp2 = Untypeast.expression exp2 in
        let lloc' = {lloc with txt = Longident.rename rename_to lloc.txt} in
        let pexp' = {pexp with pexp_desc = Pexp_field (pexp2,lloc')} in
        Rr_longident (Pexpression pexp',
                      lloc')
    | Af_typed (Texpression ({exp_desc=Texp_setfield (exp2, lloc, _, exp3); _} as exp)) ->
        let pexp = Untypeast.expression exp in
        let pexp2 = Untypeast.expression exp2 in
        let pexp3 = Untypeast.expression exp3 in
        let lloc' = {lloc with txt = Longident.rename rename_to lloc.txt} in
        let pexp' = {pexp with pexp_desc = Pexp_setfield (pexp2,lloc',pexp3)} in
        Rr_longident (Pexpression pexp',
                      lloc')
    | Af_label_typed (i, Tpattern ({pat_desc=Tpat_record (labels, closed_flag); _} as pat)) ->
        let lloc,ld,pat1 = List.nth labels i in
        let lloc' = {lloc with txt = Longident.rename rename_to lloc.txt} in
        let labels' =  List.change_nth i
            (fun (lloc, ld, pat) -> (lloc',ld,pat))
            labels in
        let ppat' = Untypeast.pattern {pat with
            pat_desc = Tpat_record (labels', closed_flag)
        } in
        Rr_ast_node (Ppattern ppat')
        (* Rr_longident (Ppattern ppat', lloc') *)
    | Af_label_typed (i, Texpression ({exp_desc=Texp_record (labels,exp2_opt);_} as exp)) ->
        let lloc,ld,pat1 = List.nth labels i in
        let lloc' = {lloc with txt = Longident.rename rename_to lloc.txt} in
        let labels' =  List.change_nth i
            (fun (lloc, ld, pat) -> (lloc',ld,pat))
            labels in
        let pexp' = Untypeast.expression {exp with
            exp_desc = Texp_record (labels', exp2_opt)
        } in
        Rr_ast_node (Pexpression pexp')
        (* Rr_longident (Pexpression pexp', lloc') *)
    | Af_typed (Tlabel_declaration ({ld_name; _} as ld)) ->
        let ld = Untypeast.label_declaration ld in
        let sloc' = {ld_name with txt = rename_to} in
        let ld' = {ld with pld_name = sloc'} in
        Rr_string (Plabel_declaration ld',
                   sloc')
    | _ -> raise Not_implemented

let rename_in_source =
    let span l = Source_change.span l.loc_start l.loc_end
    in function
    (* Pretty-print entire node *)
    | Rr_ast_node node ->
       begin match Parsetree_soup.location_of node with
       | Some loc -> span loc (Format.to_bytes (Parsetree_soup.pprint node))
       | _ -> raise Not_implemented
       end
    (* Pretty-print only the identifier *)
    | Rr_string (node, sloc) ->
        span sloc.loc sloc.txt
    | Rr_longident (node, lloc) ->
        span lloc.loc (Longident.to_string lloc.txt)