open Compilerlibs_overlay
open Stdlib_overlay
open Typedtree_soup
open Parsetree_soup
open Typedtree
open Types
open Project

(* specifying what to rename *)

let value_at_longident lident project =
    let open Symbol_reference in
    match Env.lookup_value_opt lident project.p_env with
    | Some (_, vd) ->
        Some {
            sr_desc = Sr_usage (Su_longident {
                id = lident;
                to_loc = vd.val_loc;
                from_loc = vd.val_loc;
            });
            sr_fragment = Af_none
        }
    | None -> None

let type_at_longident lident project =
    let open Symbol_reference in
    match Env.lookup_type_opt lident project.p_env with
    | Some (_, td) ->
        Some {
            sr_desc = Sr_usage (Su_longident {
                id = lident;
                to_loc = td.type_loc;
                from_loc = td.type_loc
            });
            sr_fragment = Af_none
        }
    | None -> None

(* Pseudo-longident notation: Module.type.field *)
let field_at_longident_pseudo lident project =
    let open Symbol_reference in
    match lident with
    | Longident.Ldot(lident, field_name) ->
        begin match Env.lookup_type_opt lident project.p_env with
        | Some (_, {type_loc; type_kind=Type_record (labels,_); _}) ->
            begin match
                List.find_opt (fun {ld_id; _} -> field_name = Ident.name ld_id) labels
            with Some {ld_id;ld_loc;_} ->
                Some {
                    sr_desc = Sr_declaration {
                        id = ld_id;
                        to_loc = ld_loc;
                        from_loc = ld_loc
                    };
                    sr_fragment = Af_none
                }
            | None -> None
            end
        | _ -> None
        end
    | _ -> None

(* Proper longident notation: Module.field *)
let field_at_longident lident project =
    let open Symbol_reference in
    match Env.lookup_label_opt lident project.p_env with
    | Some ({lbl_name; lbl_loc; _}) ->
        Some {
            sr_desc = Sr_usage (Su_name {
                id = lbl_name;
                to_loc = lbl_loc;
                from_loc = lbl_loc
            });
            sr_fragment = Af_none
        }
    | None -> None

(* symbol at point *)

let symbol_at_point ~f point =
    let open Location in
    let check_loc sr =
        let {loc_start; loc_end; _} =
            Symbol_reference.location_from sr
        in Point.right_of_location point loc_start &&
           Point.left_of_location point loc_end in
    let f node =
        match List.filter check_loc (f node) with
        | x::_ -> Some x
        | _ -> None
    in function
    | Ml (Typed (_, {tml_structure; _})) ->
        Typedtree_find.In_structure.first_map ~f tml_structure
    | Mli (Typed (_, {tmli_signature; _})) ->
        Typedtree_find.In_signature.first_map ~f tmli_signature
    | _ -> raise Not_implemented

let value_at_point = symbol_at_point ~f:Find_references.references_to_values
let type_at_point = symbol_at_point ~f:Find_references.references_to_types
let field_at_point = symbol_at_point ~f:Find_references.references_to_fields