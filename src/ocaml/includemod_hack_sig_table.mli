(* Clear saved coercion mapping *)
val clear : unit -> unit

(* Add a pair (given,coerce_to) of signature items to the table. *)
val add : Types.signature_item -> Types.signature_item -> unit

(* Return the saved coercion mapping as a list of pairs  *)
val to_alist : unit -> (Types.signature_item * Types.signature_item) list