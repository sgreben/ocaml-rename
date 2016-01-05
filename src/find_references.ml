open Compilerlibs_overlay
open Stdlib_overlay
open Asttypes
open Typedtree_soup
open Parsetree_soup
open Typedtree
open Types
open Project

let coercion_graph ~f project =
    Symbol_reference.Graph.of_edges
        (Project.fold_modules project ~init:[] ~f:(fun edges m ->
            (List.concat_map f m.tml_coercions) @ edges))

let find_in_project ~f reference_set project =
    let open Symbol_reference in
    let mem sr = Set_to.mem sr reference_set in
    let f node = List.filter mem (f node) in
    let signature = Typedtree_find.In_signature.all_concat_map ~f
    and structure = Typedtree_find.In_structure.all_concat_map ~f in
    let open Set_from in
    Project.fold_files project ~init:empty
        ~f:(fun acc file ->
            let in_sig =
                match file.pf_desc with
                | Ml (Typed (_, {tml_signature_mli = Some sig_mli; _})) ->
                    of_list (signature sig_mli)
                | Mli (Typed (_, {tmli_signature; _})) ->
                    of_list (signature tmli_signature)
                | _ -> empty
            and in_struct =
                match file.pf_desc with
                | Ml (Typed (_, {tml_structure; _})) ->
                    of_list (structure tml_structure)
                | _ -> empty
            in union (union in_struct in_sig) acc)

(* references to values *)

let value_coercion_graph =
    let open Symbol_reference in
    let make id loc sr_fragment = {
        sr_desc = Sr_declaration {id = id; to_loc = loc; from_loc = loc};
        sr_fragment
    } in (function
            | Sig_value (id, vd), Sig_value (id', vd') ->
                [(make id vd.val_loc (Af_sig (Sig_value (id, vd))),
                  make id' vd'.val_loc (Af_sig (Sig_value (id', vd'))))]
            | _ -> [])

let references_to_values =
    let open Symbol_reference in
    let make sr_desc sr_fragment = [{
        sr_desc;
        sr_fragment=Af_typed sr_fragment
    }] in (fun node ->
            let make sr = make sr node in
            match node with
            | Tsignature_item {sig_desc=Tsig_value vd;_} ->
                make (Sr_usage (Su_name {
                    id = vd.val_name.txt;
                    to_loc = vd.val_loc;
                    from_loc = vd.val_loc;
                }))
            | Tpattern {pat_desc=Tpat_alias (_,id,sloc); pat_loc; _} ->
                make (Sr_declaration {
                    id;
                    to_loc = pat_loc;
                    from_loc = sloc.loc;
                })
            | Tpattern {pat_desc=Tpat_var (id,sloc);pat_loc;_} ->
                make (Sr_declaration {
                    id;
                    to_loc = sloc.loc;
                    from_loc = sloc.loc
                })
            | Texpression {exp_desc=Texp_ident(path,lloc,tvd);_} ->
                make (Sr_usage (Su_path {
                    id = path;
                    to_loc = tvd.val_loc;
                    from_loc = lloc.loc
                }))
            | _ -> [])

(* references to types *)

let type_coercion_graph =
    let open Symbol_reference in
    let make id loc sr_fragment = {
        sr_desc = Sr_declaration {id = id; to_loc=loc; from_loc=loc};
        sr_fragment
    } in (fun (l, r) ->
            match l, r with
            | Sig_type (id, td, _), Sig_type (id', td', _) ->
                [(make id td.type_loc (Af_sig l),
                  make id' td'.type_loc (Af_sig r))]
            | _ -> [])

let references_to_types =
    let open Symbol_reference in
    let make sr_desc sr_fragment = [{
        sr_desc;
        sr_fragment=Af_typed sr_fragment
    }] in fun node ->
        let make sr = make sr node in match node with
        | Ttype_declaration td ->
            make (Sr_declaration {
                id = td.typ_id;
                to_loc = td.typ_loc;
                from_loc = td.typ_loc
            })
        | Tcore_type {ctyp_desc=Ttyp_constr (path,lloc,_);ctyp_env} ->
            let {type_loc; _} = Env.find_type path ctyp_env in
            make (Sr_usage (Su_longident {
                id = lloc.txt;
                to_loc = type_loc;
                from_loc = lloc.loc
            }))
        | _ -> []

(* references to record fields *)

let field_coercion_graph =
    let open Symbol_reference in
    let make id loc sr_fragment = {
        sr_desc = Sr_declaration {id = id; to_loc=loc; from_loc=loc};
        sr_fragment = Af_sig sr_fragment
    } in (fun (l,r) ->
            match l, r with
            | Sig_type (_, {type_kind=Type_record (labels,_);_}, _),
              Sig_type (_, {type_kind=Type_record (labels',_);_}, _) ->
                List.zip_with (fun ld ld' ->
                    (make ld.ld_id ld.ld_loc l, make ld'.ld_id ld'.ld_loc r))
                    labels labels'
            | _ -> [])

let references_to_fields =
    let open Symbol_reference in
    let make sr_desc sr_fragment = {
        sr_desc;
        sr_fragment=Af_typed sr_fragment
    }
    and make_label i sr_desc sr_fragment = {
        sr_desc;
        sr_fragment = Af_label_typed (i, sr_fragment)
    }
    in fun node ->
        let make sr = [make sr node]
        and make_label i lloc {lbl_name; lbl_loc; _} =
            make_label i (Sr_usage (Su_name {
                id = lbl_name;
                to_loc = lbl_loc;
                from_loc = lloc.loc
            })) node in
        match node with
        | Tpattern {pat_desc=Tpat_record (labels,_);_} ->
            labels |> List.mapi (fun i (lloc,label,_) -> make_label i lloc label)
        | Texpression {exp_desc=Texp_record (labels,_)} ->
            labels |> List.mapi (fun i (lloc,label,_) -> make_label i lloc label)
        | Texpression {exp_desc=Texp_field (_, {loc; _}, {lbl_name; lbl_loc;_})} ->
            make (Sr_usage (Su_name {id = lbl_name; to_loc=lbl_loc; from_loc=loc}))
        | Tlabel_declaration {ld_id; ld_loc; _} ->
            make (Sr_declaration {id = ld_id; to_loc=ld_loc; from_loc=ld_loc})
        | Texpression {exp_desc=Texp_setfield (_, {loc; _}, {lbl_name; lbl_loc; _}, _)} ->
            make (Sr_usage (Su_name {id = lbl_name; to_loc=lbl_loc; from_loc=loc}))
        | _ -> []
