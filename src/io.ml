let load_file f =
    let ch = open_in f in
    let n = in_channel_length ch in
    let s = Bytes.create n in
    really_input ch s 0 n;
    close_in ch;
    s

let write_file f str =
    let ch = open_out f in
    output_bytes ch str;
    close_out ch

(* Write to $f.N, where N is minimal such that $f.N does not exist. *)
let write_bak_file f str =
    let name n = if n = 0 then f else Format.sprintf "%s.%d" f n in
    let rec min_n n = if Sys.file_exists (name n) then min_n (n+1) else n in
    let rec write f n =
        let f = name n in
        try(let ch = open_out_gen [Open_wronly;Open_creat;Open_excl] 0o666 f in
            output_bytes ch str;
            close_out ch;
            f)
        with _ -> write f (n+1)
    in write f (min_n 0)


exception Make_temporary_directory_failed
let make_temporary_directory ?(max_attempts=10) prefix suffix  =
    let rec loop i =
        if i > max_attempts then raise Make_temporary_directory_failed
        else try
            let file_name = Filename.temp_file prefix suffix in
            Unix.unlink file_name;
            Unix.mkdir file_name 0o755;
            file_name
        with Unix.Unix_error _ -> loop (i+1)
    in loop 1
(* PERF: replace by mmap'ed files (Bigarray) *)