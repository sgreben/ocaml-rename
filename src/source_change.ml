open Stdlib_overlay
open Lexing

module Span = struct
    type t = {
        span_loc_start : Lexing.position;
        span_loc_end : Lexing.position;
        span_replacement : string
    }

    let to_formatter fmt {span_loc_start;span_loc_end;span_replacement} =
        let string_of_pos pos =
            let (_,l,c) = Location.get_pos_info pos
            in Format.sprintf "l%dc%d" l (c+1)
        in Format.fprintf fmt "@[replace %s to %s by @[\"%s\"@]@]"
            (string_of_pos span_loc_start)
            (string_of_pos span_loc_end)
            (String.escaped span_replacement)

    let compare = Pervasives.compare
end

open Span

type t = {
    change_file_name : string;
    change_spans : Span.t list;
}

let num_changes {change_spans; _}  = List.length change_spans

let file_name {change_file_name; _} = change_file_name

let span start_ end_ replacement = {
    span_loc_start = start_;
    span_loc_end = end_;
    span_replacement = replacement
}

let group_spans spans =
    spans
    |> List.group_by
        ~key:(fun {span_loc_start;_} -> span_loc_start.pos_fname)
        ~compare_key:Pervasives.compare
    |> List.map
        (fun (change_file_name,change_spans) ->
            {change_file_name; change_spans})

let to_formatter fmt {change_file_name;change_spans} =
    Format.fprintf fmt "@[@[%s@]:@\n  @[" change_file_name;
    change_spans |> List.iter (fun span ->
            Span.to_formatter fmt span;
            Format.fprintf fmt "@\n");
    Format.fprintf fmt "@]@]@\n"


let apply {change_file_name;change_spans} =
    match change_spans with
    | [] -> None
    | _::_ ->
        let source = Io.load_file change_file_name in
        let source_length = String.length source in
        let buffer = Buffer.create source_length in
        let paste start _end =
            let length = _end - start in
            if length > 0 then
                Buffer.add_bytes buffer (String.sub source start length) in
        let last_change = change_spans |> List.fold_left (fun last_change d ->
            let current_start = d.span_loc_start.pos_cnum in
            let current_end = d.span_loc_end.pos_cnum in
            paste last_change current_start;
            Buffer.add_bytes buffer d.span_replacement;
            current_end
        ) 0 in
        paste last_change source_length;
        Some (Buffer.to_bytes buffer)

(* PERF: perform changes on an mmap'ed file  *)
let apply_in_place change =
    match apply change with
    | Some str -> Io.write_file change.change_file_name str; true
    | None -> false