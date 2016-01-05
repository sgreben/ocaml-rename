(*
    # BUGS

    - `patch` refuses to follow symlinked directories.
      This breaks undo when files in symlinked dirs are part of the diff.

    # MISSING FEATURES

    - use expr/pat environment to qualify replacement identifiers (in case of shadowing)
    - search for .ml/.mli in include directories

    # PERF

    - replace dummy implementation in graph.ml

    # TODO

    - missing formatter cases in Parsetree_soup

*)
open Stdlib_overlay
open Cli_action_summary
open Cli_action_spec

exception Error_while_applying_changes

let dry_run changes = {
    rs_files_changed =
        List.map Source_change.file_name changes;
    rs_num_changes =
        List.map_fold_left changes
            ~init:0 ~f_map:Source_change.num_changes ~f_fold:(+);
    rs_changes = changes;
}

let in_place_no_backup changes =
    let summary = dry_run changes in
    changes |> List.iter (fun change ->
        if not (Source_change.apply_in_place change) then
            raise Error_while_applying_changes);
    summary

let in_place_with_backup changes =
    let summary = dry_run changes in
    let bs_backup_dir = Io.make_temporary_directory "ocaml_rename_" ".bak" in
    let bs_backup_mapping =
        changes |> List.map (fun change ->
            let bf_old_path = Source_change.file_name change in
            let f_base = Filename.basename bf_old_path in
            let bf_new_path = Filename.concat bs_backup_dir f_base in
            let old_source = Io.load_file bf_old_path in
            let bf_new_path = Io.write_bak_file bf_new_path old_source in
            change, {bf_new_path; bf_old_path})
    in begin
        try bs_backup_mapping |> List.iter (fun (change,{bf_old_path; _}) ->
            match Source_change.apply change with
            | Some patched_source ->
                Io.write_file bf_old_path patched_source
            | None -> raise Error_while_applying_changes)
        with Error_while_applying_changes ->
            Format.fprintf Format.err_formatter
                "@[Error while applying changes@]@\n";
    end;
    let bs_backup_mapping = List.map snd bs_backup_mapping
    in summary, {bs_backup_dir; bs_backup_mapping}

let generate_undo_patch {bs_backup_dir; bs_backup_mapping} =
    let us_patch_path = bs_backup_dir^".undo.diff" in
    let us_command = "patch -p0 <"^us_patch_path in
    bs_backup_mapping |> List.iter (fun {bf_old_path; bf_new_path} ->
        ignore @@ Shell.diff_append ~left:bf_old_path
                                    ~right:bf_new_path
                                    us_patch_path);
    {us_patch_path; us_command}

let perform_cli_action {a_desc; a_extra} project =
    let a_desc, spec, rename_to = a_desc in
    let changes = Cli_action.of_desc a_desc spec rename_to project in
    match a_extra.ae_mode with
    | Am_dry ->
        As_dry_run (dry_run changes)
    | Am_inplace ->
        As_in_place (in_place_no_backup changes, None, None)
    | Am_inplace_backup ->
        let rs, bs = in_place_with_backup changes in
        As_in_place (rs, Some bs, None)
    | Am_inplace_backup_patch ->
        let rs, bs = in_place_with_backup changes in
        let us = generate_undo_patch bs in
        As_in_place (rs, Some bs, Some us)

let main () =
    match Cli_args.parse () with
    | Some (action, source_files) ->
        let project =
            try Compiler_frontend.load_project source_files
            with e ->
                Location.report_exception Format.err_formatter e;
                raise e in
        let action_summary = perform_cli_action action project in
        Cli_action.print_action_summary
            action.a_extra.ae_summary_length
            action_summary
    | _ -> Cli_args.print_usage ()

let _ = main ()