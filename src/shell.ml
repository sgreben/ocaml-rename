let run_and_collect_stdout command =
    let lines = ref [] in
    let ch = Unix.open_process_in command in
    let finish () =
        let status = Unix.close_process_in ch in
        status, List.rev !lines
    in try
        while true; do lines := input_line ch :: !lines done;
        finish ()
    with End_of_file -> finish ()

let diff_append ~left ~right to_dest =
    Sys.command @@ String.concat " " [
        "diff"; "-u";
        Filename.quote left;
        Filename.quote right;
        ">>";Filename.quote to_dest;
    ]

let ocamlfind_query package =
    run_and_collect_stdout @@ String.concat " " ["ocamlfind"; "query"; package]
    |> function
    | Unix.WEXITED 0, [path] -> Some path
    | _ -> None