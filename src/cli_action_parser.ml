open Stdlib_overlay
open Cli_action_spec

let parse_point point_string =
    let i = int_of_string in
    match String.split ~on:':' point_string with
    | [file;line;column] -> Some (Point.({file;line=i line;column=i column}))
    | _ -> None

let parse_symbol_spec = function
    | "at"::point_string::rest ->
        let point_opt =
            match parse_point point_string with
            | Some point -> Some (Ss_point point)
            | None -> None
        in point_opt, rest
    | point_string::rest when point_string.[0]='@' ->
        let point_string =
            (String.sub point_string 1 (String.length point_string - 1)) in
        let point_opt =
            match parse_point point_string with
            | Some point -> Some (Ss_point point)
            | None -> None
        in point_opt, rest
    | lident_string::rest ->
        let lident_opt =
            try Some (Ss_longident (Longident.parse lident_string)) with _ -> None
        in lident_opt, rest
    | args -> None, args

let parse_action_desc = function
    | ("value"|"valu"|"val"|"va"|"v") -> Some Ad_rename_value
    | ("type"|"typ"|"ty"|"t") -> Some Ad_rename_type
    | ("field"|"fiel"|"fie"|"fi"|"f") -> Some Ad_rename_field
    | _ ->  None

let parse_action = function
    | action_string::rest ->
        begin match parse_action_desc action_string with
        | Some action_desc ->
            begin match parse_symbol_spec rest with
            | Some symbol_spec, rest ->
                begin match rest with
                | rename_to::rest -> Some (action_desc, symbol_spec, rename_to), rest
                | _ -> None, rest
                end
            | None, rest -> None, rest
            end
        | None -> None, rest
        end
    | args -> None, args
