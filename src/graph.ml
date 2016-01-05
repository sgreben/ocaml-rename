module type Vertex = sig
    type id
    type t
    val id : t -> id
    val compare_id : id -> id -> int
end

(* slow but trivial *)
module Make(V:Vertex) = struct

    type t = {edges: (V.t*V.t) list}

    let empty = {edges = []}

    let of_edges edges = {edges}

    let add_edge e {edges} = {edges = e::edges}

    module Set = Set.Make(struct
        type t = V.id
        let compare = V.compare_id
    end)

    let reach_from v {edges} =
        let rec reach (reached_ids, reached_vs) =
            let reached v = Set.mem (V.id v) reached_ids in
            let delta_ids,delta_vs =
                edges |> List.fold_left (fun ((acc_id, acc_v) as acc)  (v, v') ->
                    let reached v = Set.mem (V.id v) acc_id || reached v in
                    if reached v && not (reached v') then
                        (Set.add (V.id v') acc_id, v'::acc_v)
                    else if reached v' && not (reached v) then
                        (Set.add (V.id v) acc_id, v::acc_v)
                    else acc) (Set.empty,[]) in
            if Set.is_empty delta_ids then reached_vs
            else reach (Set.union reached_ids delta_ids,
                        delta_vs @ reached_vs)
        in reach (Set.singleton (V.id v), [v])

end