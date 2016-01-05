open Typedtree
open Location
open Ident

let rename_pattern rename_to ({pat_desc; _} as pat) =
    match pat_desc with
    | Tpat_var (id, {txt;loc}) ->
        Some {
            pat with
            pat_desc=Tpat_var({id with name=rename_to}, {loc;txt=rename_to})
        }
    | _ -> None