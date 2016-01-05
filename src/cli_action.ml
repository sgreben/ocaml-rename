open Project
open Symbol_reference
open Point
open Cli_action_spec
open Cli_action_summary

exception No_symbol_found
exception No_such_file_loaded

let format_run_summary fmt rs = function
    | Sl_very_short -> format_run_summary_very_short fmt rs
    | Sl_short -> format_run_summary_short fmt rs
    | Sl_medium -> format_run_summary_medium fmt rs

let format_action_summary fmt length =
    let run_summary rs =
        Format.fprintf fmt "*** Run summary:@\n  @[";
        format_run_summary fmt rs length;
        Format.fprintf fmt "@]@\n"
    and backup_summary bs =
        Format.fprintf fmt "*** Backup summary@\n  @[";
        format_backup_summary fmt bs;
        Format.fprintf fmt "@]@\n"
    and undo_summary us =
        Format.fprintf fmt "*** Undo summary@\n  @[";
        format_undo_summary fmt us;
        Format.fprintf fmt "@]@\n"
    in function
    | As_dry_run rs ->
        run_summary rs
    | As_in_place (rs,None,None) ->
        run_summary rs
    | As_in_place (rs,Some bs, None) ->
        run_summary rs;
        backup_summary bs
    | As_in_place (rs,Some bs, Some us) ->
        run_summary rs;
        backup_summary bs;
        undo_summary us
    | As_in_place (rs, None, Some us) ->
        run_summary rs;
        undo_summary us

let print_action_summary length action_summary =
    Format.fprintf Format.std_formatter "@[";
    format_action_summary Format.std_formatter length action_summary;
    Format.fprintf Format.std_formatter "@]"

let rename spec rename_to project ~at_point
                                  ~at_longident
                                  ~coercion_edge
                                  ~find_symbol_references
                                  ~rename_reference =
    let symbol_reference_opt =
        match spec with
        | Ss_point point ->
            begin match Project.file point.file project with
            | Some file -> at_point point file.pf_desc
            | None -> raise No_such_file_loaded
            end
        | Ss_longident lident -> at_longident lident project in
    match symbol_reference_opt with
    | Some symbol_reference ->
        let coercion_graph = Find_references.coercion_graph ~f:coercion_edge project in
        let coercion_closure =
            Graph.reach_from symbol_reference coercion_graph in
        let reference_set =
            Set_to.add symbol_reference (Set_to.of_list coercion_closure) in
        let reference_set =
            Find_references.find_in_project reference_set ~f:find_symbol_references project in
        let rename_ops =
            List.map (rename_reference rename_to) (Set_from.elements reference_set) in
        let changes = List.map Rename.rename_in_source rename_ops in
        Source_change.group_spans changes
    | None -> raise No_symbol_found

let rename_value spec rename_to project =
    rename ~at_point:Symbol_lookup.value_at_point
           ~at_longident:Symbol_lookup.value_at_longident
           ~coercion_edge:Find_references.value_coercion_graph
           ~find_symbol_references:Find_references.references_to_values
           ~rename_reference:Rename.rename_value_reference
           spec rename_to project

let rename_type spec rename_to project =
    rename ~at_point:Symbol_lookup.type_at_point
           ~at_longident:Symbol_lookup.type_at_longident
           ~coercion_edge:Find_references.type_coercion_graph
           ~find_symbol_references:Find_references.references_to_types
           ~rename_reference:Rename.rename_type_reference
           spec rename_to project

let rename_field spec rename_to project =
    rename ~at_point:Symbol_lookup.field_at_point
           ~at_longident:Symbol_lookup.field_at_longident
           ~coercion_edge:Find_references.field_coercion_graph
           ~find_symbol_references:Find_references.references_to_fields
           ~rename_reference:Rename.rename_field_reference
           spec rename_to project

let of_desc = function
  | Ad_rename_value -> rename_value
  | Ad_rename_type -> rename_type
  | Ad_rename_field -> rename_field