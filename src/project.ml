open Stdlib_overlay

type ('parsed, 'typed) project_file_state =
    | Parsed of 'parsed * exn
    | Typed of 'parsed * 'typed

type typed_mli = {
    tmli_module_id: Ident.t;
    tmli_signature : Typedtree.signature;
}

type typed_ml = {
    tml_module_id: Ident.t;
    tml_structure : Typedtree.structure;
    tml_signature_inferred : Types.signature;
    tml_signature_mli : Typedtree.signature option;
    tml_module_coercion : Typedtree.module_coercion;
    tml_coercions : (Types.signature_item * Types.signature_item) list;
}

type project_file_desc =
    | Ml of (Parsetree.structure, typed_ml) project_file_state
    | Mli of (Parsetree.signature, typed_mli) project_file_state

type project_file = {
    pf_path : string;
    pf_desc : project_file_desc;
}

type project = {
    p_files : project_file list;
    p_env : Env.t
}

let file path {p_files; _} =
    List.find_opt (fun {pf_path; _} -> pf_path = path) p_files

let fold_files ~f ~init {p_files; _} =
    List.fold_left f init p_files

let fold_modules ~f ~init =
    fold_files ~init ~f:(fun acc file ->
        match file.pf_desc with
        | Ml (Typed (_, typed_module)) -> f acc typed_module
        | _ -> acc)