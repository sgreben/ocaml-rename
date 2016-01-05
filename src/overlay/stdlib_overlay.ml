exception Not_implemented

module List = struct
    include List

    let zip_with f =
        let rec loop acc xs ys =
            match xs,ys with
            | x::xs, y::ys -> loop (f x y :: acc) xs ys
            | _ -> rev acc
        in loop []

    let map_fold_left ~f_fold ~f_map ~init =
        fold_left (fun acc x -> f_fold acc (f_map x)) init

    let filter_map f = fold_left (fun acc x ->
            match f x with
            | Some y -> y::acc
            | None -> acc) []

    let concat_map f =
        let rec loop acc = function
            | [] -> rev acc
            | x :: xs -> (match f x with
                | [] -> loop acc xs
                | fx -> loop (rev_append fx acc) xs)
        in loop []

    let group_by ~key ~compare_key xs =
        let compare x y = compare_key (key x) (key y) in
        let equal_key x y = compare x y = 0 in
        let a = Array.of_list xs in
        Array.sort compare a;
        let groups =
            Array.fold_left (fun (current, rest) x ->
                 match current with
                 | Some (current_key,current_group) ->
                     if key x = current_key then
                         (Some (current_key,x::current_group), rest)
                     else
                         (Some (key x,[x]), (current_key,current_group)::rest)
                 | None -> Some(key x,[x]), rest
            ) (None,[]) a in
        match groups with
        | Some (key,group), rest -> (key, group)::rest
        | None, rest -> rest

    let max_by f xs =
        let max_opt = fold_left (fun acc x ->
            match acc with
            | Some (max_so_far, x_max) ->
                let fx = f x in if fx >= max_so_far then Some (fx, x)
                else acc
            | None ->  Some (f x, x))
            None xs
        in match max_opt with
        | Some (_,x) -> Some x
        | None -> None

    let change_nth n f xs =
        let rec loop acc i = function
            | x::xs ->
                if i = 0 then loop ((f x) :: acc) (i-1) xs
                else if i > 0 then loop (x::acc) (i-1) xs
                else rev_append xs acc
            | [] -> acc
        in rev (loop [] n xs)

    let find_opt f xs =
        try Some (List.find f xs) with Not_found -> None

end

module String = struct
    include String
    module Map = Map.Make(String)

    let split ~on str =
        let index_from_opt i =
            try Some (index_from str i on)
            with Not_found -> None in
        let rec loop i acc = match index_from_opt i with
        | Some j ->
            let w = sub str i (j - i) in
            loop (j+1) (w::acc)
        | None ->
            let w = sub str i (String.length str - i) in
            List.rev (w::acc)
        in loop 0 []

    let capitalize_ascii = capitalize
end

module Format = struct
    include Format
    let to_bytes : (Format.formatter -> unit) -> bytes  =
        let buffer = Buffer.create 80 in
        let formatter = Format.formatter_of_buffer buffer in
        (fun f ->
            f formatter;
            Format.pp_print_flush formatter ();
            let bytes = Buffer.to_bytes buffer in
            Buffer.clear buffer;
            bytes)
end

module Sys = struct
    include Sys

    let command =
      match Sys.os_type with
      | "Win32" -> fun cmd ->
          if cmd = "" then 0 else
          let cmd = "bash --norc -c "^Filename.quote cmd in
          Sys.command cmd
      | _ -> fun cmd -> if cmd = "" then 0 else Sys.command cmd

end