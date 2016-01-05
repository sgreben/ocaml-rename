open Compilerlibs_overlay
open Typedtree_soup
open Parsetree_soup

type 'a with_locations = {
    (* Location of the reference *)
    from_loc : Location.t;
    (* Location of the declaration *)
    to_loc : Location.t;
    (* The declaration's identifier *)
    id : 'a
}

type usage =
    | Su_direct of Ident.t with_locations
    | Su_path of Path.t with_locations
    | Su_longident of Longident.t with_locations
    | Su_name of string with_locations

type desc =
    | Sr_declaration of Ident.t with_locations
    | Sr_usage of usage

type ast_fragment =
    | Af_typed of typedtree_node
    | Af_label_typed of int * typedtree_node
    | Af_untyped of parsetree_node
    | Af_label_untyped of int * parsetree_node
    | Af_sig of Types.signature_item
    | Af_none

module T = struct
    type t = {
        sr_desc : desc;
        sr_fragment : ast_fragment;
    }
end
include T

let rec usage_location_to = function
    | Su_direct {to_loc; _}
        | Su_path {to_loc; _}
        | Su_longident {to_loc; _}
        | Su_name {to_loc; _} ->
        to_loc

let rec usage_location_from = function
    | Su_direct {from_loc; _}
        | Su_path {from_loc; _}
        | Su_longident {from_loc; _}
        | Su_name {from_loc; _} ->
        from_loc

let desc_location_to = function
    | Sr_declaration {to_loc; _} -> to_loc
    | Sr_usage usage -> usage_location_to usage

let desc_location_from = function
    | Sr_declaration {from_loc; _} -> from_loc
    | Sr_usage usage -> usage_location_from usage

let location_to {sr_desc; _} = desc_location_to sr_desc
let location_from {sr_desc; _} = desc_location_from sr_desc

let compare_location_to u u' = compare (location_to u) (location_to u')
let compare_location_from u u' = compare (location_from u) (location_from u')

module Set_to = Set.Make(struct
    type t = T.t
    let compare = compare_location_to
end)

module Set_from = Set.Make(struct
    type t = T.t
    let compare = compare_location_from
end)

module Graph = Graph.Make(struct
    type t = T.t
    type id = Location.t
    let id = location_to
    let compare_id = Location.compare
end)