module L = Nonempty_list

type 'a t = {
    neq_enqueue : 'a L.nonempty_list option;
    neq_dequeue: 'a L.nonempty_list;
}

type 'a dequeue_result = {
    dr_element: 'a;
    dr_queue: 'a t option;
}

let enqueue ({neq_enqueue; _} as q) x =
    let neq_enqueue =
        match neq_enqueue with
        | None -> Some (L.singleton x)
        | Some xs -> Some (L.cons x xs)
    in {q with neq_enqueue}

let dequeue ({neq_enqueue; neq_dequeue} as q) =
    match neq_dequeue with
    | L.End dr_element ->
        {
            dr_element;
            dr_queue =
                match neq_enqueue with
                | None -> None
                | Some l -> Some {neq_dequeue=L.rev l; neq_enqueue=None}
        }
    | L.Cons (dr_element, neq_dequeue) ->
        {dr_element; dr_queue=Some {q with neq_dequeue}}