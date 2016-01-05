open Cli_action_spec

type options = {
    o_disable_backup : bool ref;
    o_disable_undo : bool ref;
    o_dry_run : bool ref;
    o_summary_length : summary_length ref;
}

let options = {
    o_disable_backup = ref false;
    o_disable_undo = ref false;
    o_dry_run = ref false;
    o_summary_length = ref Sl_short
}

let package_list = ref []

let usage = String.concat "\n" [
    "Usage:";
    "  ocaml-rename [options] [command] [source files in dependency order]";
    "";
    "Commands:";
    "  value at <value-loc> <new-id>";
    "  value @<value-loc> <new-id>";
    "  value <value> <new-id>";
    "";
    "  type at <type-loc> <new-id>";
    "  type @<type-loc> <new-id>";
    "  type <type> <new-id>";
    "";
    "  field at <field-loc> <new-id>";
    "  field @<field-loc> <new-id>";
    "  field <field> <new-id>";
    "";
    "Options:";
]

let add_to_list li s = li := s :: !li

let spec = Arg.align [
    "-I", Arg.String (add_to_list Clflags.include_dirs),
        "<dir> Add <dir> to the list of include directories\n";
    "-package", Arg.String (add_to_list package_list),
        "<pkg> Find <pkg> using `ocamlfind query` and add its include directory\n";
    "--no-backup", Arg.Set options.o_disable_backup,
        " Disable backups (implies --no-undo) (default: make backups)\n";
    "--no-undo", Arg.Set options.o_disable_undo,
        " Disable undo patch generation (default: generate undo patch)\n";
    "--dry-run", Arg.Set options.o_dry_run,
        " Perform a dry run (imples --no-backup, --no-undo) (default: false)\n";
    "-v", Arg.Symbol (["0";"1";"2"], fun i -> options.o_summary_length := match i with
                   | "0" -> Sl_very_short
                   | "1" -> Sl_short
                   | _ -> Sl_medium),
        " Set verbosity level (default:2)\n"
]

exception Bad_package
let parse () =
    let rest = ref [] in
    Arg.parse spec (add_to_list rest) usage;
    List.rev !package_list
        |> List.iter (fun package ->
            match Shell.ocamlfind_query package with
            | Some path -> add_to_list Clflags.include_dirs path
            | None -> raise Bad_package);
    match Cli_action_parser.parse_action (List.rev !rest) with
    | Some a_desc, source_files ->
        let action = {
            a_desc = a_desc;
            a_extra = {
                ae_mode =
                    begin match !(options.o_disable_backup),
                                !(options.o_disable_undo),
                                !(options.o_dry_run) with
                    | _     , _,     true  -> Am_dry
                    | true  , _,     _     -> Am_inplace
                    | false , true,  _     -> Am_inplace_backup
                    | false , false, _     -> Am_inplace_backup_patch
                    end;
                ae_summary_length = !(options.o_summary_length)
            }
        } in Some (action, source_files)
    | _ -> None

let print_usage () = Arg.usage spec usage