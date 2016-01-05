module Location = struct
    open Lexing
    module T = struct
        include Location
        let compare = compare
    end
    include T

    module Set = Set.Make(T)

    let to_string {loc_start;loc_end;loc_ghost} =
        let s (f,l,c) = Format.sprintf "(%s:%d:%d)" f l c in
        Format.sprintf "{%s;%s;%b}"
            (s (get_pos_info loc_start))
            (s (get_pos_info loc_end))
            loc_ghost

    let length {loc_start;loc_end;_} =
        loc_end.pos_cnum - loc_start.pos_cnum

    module Actual_position = struct
        open Lexing
        type offset_only = {
            pos_fname: string;
            pos_cnum: int;
        }
        type t =
            | Correct of position
            | Offset_only of offset_only

        let of_position ({pos_fname;pos_cnum;pos_lnum;_} as p) =
            let pos_fname =
                if pos_fname = "" then !input_name else pos_fname in
            if pos_lnum = -1
            then Offset_only {pos_fname;pos_cnum}
            else Correct p
    end
end

module Longident = struct
    include Longident

    exception Cannot_rename_Lapply

    let rename rename_to = function
       | Lident _ -> Lident rename_to
       | Ldot (t,_) -> Ldot (t,rename_to)
       | Lapply _ -> raise Cannot_rename_Lapply

   let rec to_string = function
       | Lident s -> s
       | Ldot (t,s) ->
           Format.sprintf "%s.%s" (to_string t) s
       | Lapply (t,t') ->
           Format.sprintf "(%s)(%s)" (to_string t) (to_string t')
end
module Ident = struct
    module T = struct
        include Ident
        let compare = compare
    end
    include T

    module Set = Set.Make(T)
    let to_string = unique_name
end
module Path = struct
    include Path
    let to_string path =
        let rec loop = function
            | Pident id -> Ident.to_string id
            | Pdot (t,s,i) -> Format.sprintf "%s.%s[%d]" (loop t) s i
            | Papply(t,t') -> Format.sprintf "(%s)(%s)" (loop t) (loop t')
        in loop path
end
module Env = struct
    include Env
    let try_to_opt f = try Some (f ()) with _ -> None

    let find_module_opt path env = try_to_opt (fun () -> find_module path env)
    let find_value_opt path env = try_to_opt (fun () -> find_value path env)
    let find_type_opt path env = try_to_opt (fun () -> find_type path env)

    let lookup_module_opt lident env = try_to_opt (fun () -> lookup_module lident env)
    let lookup_value_opt lident env = try_to_opt (fun () -> lookup_value lident env)
    let lookup_type_opt lident env = try_to_opt (fun () -> lookup_type lident env)
    let lookup_label_opt lident env = try_to_opt (fun () -> lookup_label lident env)

end
module Untypeast = struct
    include Untypeast
    (* 4.03 *)
    (* let pattern = default_mapper.pat default_mapper
    let expression = default_mapper.expr default_mapper
    let value_description = default_mapper.value_description default_mapper
    let signature_item = default_mapper.signature_item default_mapper
    let type_declaration = default_mapper.type_declaration default_mapper
    let core_type = default_mapper.typ default_mapper
    let label_declaration = default_mapper.label_declaration default_mapper *)
    let pattern = untype_pattern
    let expression = untype_expression
    let value_description = untype_value_description
    let signature_item = untype_signature_item
    let type_declaration = untype_type_declaration
    let core_type = untype_core_type
    let label_declaration = untype_label_declaration
end