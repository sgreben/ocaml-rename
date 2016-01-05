type run_summary = {
    rs_files_changed : string list;
    rs_num_changes: int;
    rs_changes: Source_change.t list;
}

type backup_file = {
    bf_old_path : string;
    bf_new_path: string;
}

type undo_summary = {
    us_patch_path : string;
    us_command : string;
}

type backup_summary = {
    bs_backup_dir: string;
    bs_backup_mapping: backup_file list;
}

type action_summary =
    | As_dry_run of run_summary
    | As_in_place of run_summary * backup_summary option * undo_summary option

let format_run_summary_very_short fmt {rs_files_changed; rs_num_changes; _} =
    let num_files = List.length rs_files_changed in
    Format.fprintf fmt "@[%d change(s) in %d file(s)@]@\n"
        rs_num_changes
        num_files

let format_run_summary_short fmt ({rs_files_changed; rs_num_changes; rs_changes} as summary) =
    format_run_summary_very_short fmt summary;
    Format.fprintf fmt "@[";
    rs_changes |> List.iter (fun change ->
        Format.fprintf fmt "@[%s: %d changes@]@\n"
            (Source_change.file_name change)
            (Source_change.num_changes change));
    Format.fprintf fmt "@]"

let format_run_summary_medium fmt ({rs_files_changed; rs_num_changes; rs_changes} as summary) =
    format_run_summary_very_short fmt summary;
    Format.fprintf fmt "@[";
    List.iter (Source_change.to_formatter fmt) rs_changes;
    Format.fprintf fmt "@]"

let format_backup_summary fmt {bs_backup_dir; bs_backup_mapping} =
    Format.fprintf fmt "@[backup directory: %s@]@\n@["
        bs_backup_dir;
    bs_backup_mapping |> List.iter (fun {bf_old_path; bf_new_path} ->
            Format.fprintf fmt "@[%s --> %s@]@\n"
                (Filename.quote bf_old_path)
                (Filename.quote bf_new_path));
    Format.fprintf fmt "@]"


let format_undo_summary fmt {us_patch_path; us_command; _} =
    Format.fprintf fmt "@[To undo, run:@\n  @[%s@]@]"
        us_command