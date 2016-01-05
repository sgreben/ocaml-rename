type 'a nonempty_list =
    | End of 'a
    | Cons of 'a * ('a nonempty_list)

let singleton x = End x

let cons x xs = Cons (x, xs)

let head = function End x | Cons (x, _) -> x

let tail = function End _ -> None | Cons (_, xs) -> Some xs

let fold f init =
    let rec loop acc = function
        | End x -> f acc x
        | Cons (x, xs) -> loop (f acc x) xs
    in loop init

let iter f = fold (fun () x -> f x) ()

let rev = function
    | End x -> End x
    | Cons (x, xs) -> fold (fun acc x -> Cons(x, acc)) (End x) xs

let to_bytes f xs =
    let b = Buffer.create 127 in
    ignore @@ fold (fun first x ->
        let first = first || (
            Buffer.add_bytes b "; ";
            true)
        in Buffer.add_bytes b (f x);
           first) false xs;
    Buffer.to_bytes b
