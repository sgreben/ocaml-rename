open Asttypes
open Typedtree
open Typedtree_soup

exception Break

let first ~f ~iter x =
    let out_result = ref None in
    begin
        try iter ~enter:(fun x -> if f x then (out_result := Some x; raise Break))
                 ~leave:ignore
                 x
        with Break -> ()
    end;
    !out_result

let first_map ~f ~iter x =
    let out_result = ref None in
    begin
        try iter ~enter:(fun x -> match f x with Some x -> (out_result := Some x; raise Break) | _ -> ())
                 ~leave:ignore
                 x
        with Break -> ()
    end;
    !out_result

let all ~f ~iter x =
    let out_result = ref [] in
    iter ~enter:(fun x -> if f x then out_result := x::!out_result)
         ~leave:ignore
         x;
    !out_result

let all_map ~f ~iter x =
    let out_result = ref [] in
    iter ~enter:(fun x -> match f x with Some x -> out_result := x::!out_result | _ -> ())
         ~leave:ignore
         x;
    !out_result

let all_concat_map ~f ~iter x =
    let out_result = ref [] in
    iter ~enter:(fun x -> match f x with [] -> () | fx -> out_result := fx @ !out_result)
         ~leave:ignore
         x;
    !out_result

module In_signature = struct
    let first ~f = first ~f ~iter:Typedtree_soup.iter_signature
    let first_map ~f = first_map ~f ~iter:Typedtree_soup.iter_signature
    let all ~f = all ~f ~iter:Typedtree_soup.iter_signature
    let all_map ~f = all_map ~f ~iter:Typedtree_soup.iter_signature
    let all_concat_map ~f = all_concat_map ~f ~iter:Typedtree_soup.iter_signature
end

module In_structure = struct
    let first ~f = first ~f ~iter:Typedtree_soup.iter_structure
    let first_map ~f = first_map ~f ~iter:Typedtree_soup.iter_structure
    let all ~f = all ~f ~iter:Typedtree_soup.iter_structure
    let all_map ~f = all_map ~f ~iter:Typedtree_soup.iter_structure
    let all_concat_map ~f = all_concat_map ~f ~iter:Typedtree_soup.iter_structure
end

module In_expression = struct
    let first ~f = first ~f ~iter:Typedtree_soup.iter_expression
    let first_map ~f = first_map ~f ~iter:Typedtree_soup.iter_expression
    let all ~f = all ~f ~iter:Typedtree_soup.iter_expression
    let all_map ~f = all_map ~f ~iter:Typedtree_soup.iter_expression
    let all_concat_map ~f = all_concat_map ~f ~iter:Typedtree_soup.iter_expression
end