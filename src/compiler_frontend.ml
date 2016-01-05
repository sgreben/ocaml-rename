open Compilerlibs_overlay
open Stdlib_overlay
open Asttypes
open Typedtree_soup
open Parsetree_soup
open Typedtree
open Types
open Config
open Clflags
open Misc
open Typemod_hack
open Project

let tool_name = "ocaml-rename"

let _ = Clflags.dont_write_files := true

let init_path () = (* ocamldoc/odoc_analyse.ml *)
    load_path := "" :: List.rev (Config.standard_library :: !Clflags.include_dirs);
    Env.reset_cache ()

let initial_env modules () = (* ocamldoc/odoc_analyse.ml *)
    let initial =
        if !Clflags.unsafe_string
        then Env.initial_unsafe_string
        else Env.initial_safe_string in
    try let env =
            if !Clflags.nopervasives then initial
            else Env.open_pers_signature "Pervasives" initial in
        modules |> List.fold_left (fun env (m_id, m_typ) ->
            Env.add_module m_id m_typ env) env
    with Not_found -> fatal_error "cannot open pervasives.cmi"

module Typemod_private = struct (* typing/typemod.ml *)
    let rec normalize_modtype env = function
        | Mty_ident p -> ()
        | Mty_alias p -> ()
        | Mty_signature sg -> normalize_signature env sg
        | Mty_functor(id, param, body) -> normalize_modtype env body

    and normalize_signature env = List.iter (normalize_signature_item env)

    and normalize_signature_item env = function
        | Sig_value(id, desc) -> Ctype.normalize_type env desc.val_type
        | Sig_module(id, md, _) -> normalize_modtype env md.md_type
        | _ -> ()
end

let mli_path_of_ml_path source_file =
    Misc.chop_extension_if_any source_file ^ !Config.interface_suffix

let type_implementation source_file module_name env_initial ast = (* typing/typemod.ml *)
    let open Typedtree in
    Typecore.reset_delayed_checks ();
    Env.reset_required_globals ();
    let module_id = Ident.create_persistent module_name in
    let structure, sig_inferred, env_final =
        type_structure env_initial ast (Location.in_file source_file) in
    let sig_simple_inferred = simplify_signature sig_inferred in
    let source_interface_file = mli_path_of_ml_path source_file in
    let module_coercion, sig_mli_opt, mli_opt =
        if Sys.file_exists source_interface_file then
            let ast =
                Pparse.file ~tool_name Format.err_formatter source_interface_file
                    Parse.interface ast_intf_magic_number in
            let mli_sg = Typemod_hack.type_interface env_initial ast in
            let coercion = Includemod_hack.compunit env_initial
                source_file sig_inferred "(.mli signature)" mli_sg.sig_type in
            coercion, Some mli_sg, Some {
                pf_path = source_interface_file;
                pf_desc = Mli (Typed (ast, {
                    tmli_module_id = module_id;
                    tmli_signature = mli_sg;
                }))
            }
        else begin
            (* Typemod_hack.check_nongen_schemes env_final sig_inferred; *)
            Typemod_private.normalize_signature env_final sig_simple_inferred;
            let coercion = Includemod_hack.compunit env_initial
                source_file sig_inferred "(inferred signature)" sig_simple_inferred in
            Typecore.force_delayed_checks ();
            coercion, None, None
        end
    in {
      tml_module_id = module_id;
      tml_structure = structure;
      tml_signature_inferred = sig_simple_inferred;
      tml_signature_mli = sig_mli_opt;
      tml_module_coercion = module_coercion;
      tml_coercions = Includemod_hack_sig_table.to_alist ();
    }, mli_opt


let module_of_filename source_file = (* driver/compenv.ml *)
    let basename = Filename.basename source_file in
    let name =
        try let pos = String.index basename '.' in
                String.sub basename 0 pos
        with Not_found -> basename in
    let name = String.capitalize_ascii name in
    name

let process_implementation_file modules source_file = (* ocamldoc/odoc_analyse.ml *)
    init_path ();
    Includemod_hack_sig_table.clear ();
    let module_name = module_of_filename source_file in
    Env.set_unit_name module_name;
    let env = initial_env modules () in
    let parsetree =
        Pparse.file ~tool_name Format.err_formatter source_file
            Parse.implementation ast_impl_magic_number in
    try
        let typed_ml, typed_mli_opt =
            type_implementation source_file module_name env parsetree in
        Warnings.check_fatal ();
        Ml (Typed (parsetree, typed_ml)), typed_mli_opt
    with exn ->
        Location.report_exception Format.err_formatter exn;
        Ml (Parsed (parsetree, exn)), None

let process_interface_file modules source_file = (* ocamldoc/odoc_analyse.ml *)
    init_path ();
    Includemod_hack_sig_table.clear ();
    let module_name = module_of_filename source_file in
    let module_id = Ident.create_persistent module_name in
    let env_initial = initial_env modules () in
    Env.set_unit_name module_name;
    let parsetree =
        Pparse.file ~tool_name Format.err_formatter source_file
            Parse.interface ast_intf_magic_number in
    try
        let mli_signature = Typemod_hack.type_interface env_initial parsetree in
        Warnings.check_fatal ();
        Mli (Typed (parsetree, {
                tmli_module_id = module_id;
                tmli_signature = mli_signature
        }))
    with exn ->
        Location.report_exception Format.err_formatter exn;
        Mli (Parsed (parsetree, exn))


exception Unexpected_file_extension

let process_non_source_file name =
    if Filename.check_suffix name ".cmo"
    || Filename.check_suffix name ".cma"
    || Filename.check_suffix name ".cmxa"
    || Filename.check_suffix name ".cmxs"
    || Filename.check_suffix name ".cmi" then begin
        objfiles := name :: !objfiles
    end else if Filename.check_suffix name Config.ext_dll then begin
        dllibs := name :: !dllibs
    end else raise Unexpected_file_extension

let process_files source_files =
    let defines_new_module = function
        | Ml (Typed (_, {
                tml_module_id;
                tml_signature_inferred;
                tml_signature_mli; _
            })) ->
            Some (tml_module_id,
                  Mty_signature (match tml_signature_mli with
                    | Some sig_mli -> sig_mli.sig_type
                    | None -> tml_signature_inferred))
        | _ -> None in
    Compmisc.init_path false;
    List.fold_left (fun (modules, project_files) source_file ->
        Location.input_name := source_file;
        try process_non_source_file source_file; modules, project_files
        with Unexpected_file_extension ->
        let project_file_desc, typed_mli_desc =
            if Filename.check_suffix source_file ".ml"
            || Filename.check_suffix source_file ".mlt" then
                process_implementation_file modules source_file
            else if Filename.check_suffix source_file !Config.interface_suffix then
                process_interface_file modules source_file, None
            else raise Unexpected_file_extension in
        let modules = match defines_new_module project_file_desc with
            | Some m -> m::modules
            | None -> modules in
        let pf = {
            pf_path = source_file;
            pf_desc = project_file_desc
        } in
        let project_files =
            match typed_mli_desc with
            | None -> pf::project_files
            | Some mli_pf -> mli_pf::pf::project_files in
        modules, project_files) ([], []) source_files

let load_project source_files =
    let modules, p_files = process_files source_files in
    let p_env = begin
            init_path ();
            initial_env modules ()
        end in
    { p_files; p_env }