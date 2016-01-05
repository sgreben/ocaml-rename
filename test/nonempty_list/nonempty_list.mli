type 'a nonempty_list =
    | End of 'a
    | Cons of 'a * ('a nonempty_list)

val singleton : 'a -> 'a nonempty_list

val cons : 'a -> 'a nonempty_list -> 'a nonempty_list

val head : 'a nonempty_list -> 'a

val tail : 'a nonempty_list -> 'a nonempty_list option

val fold : ('b -> 'a -> 'b) -> 'b -> 'a nonempty_list -> 'b

val iter : ('a -> unit) -> 'a nonempty_list -> unit

val rev : 'a nonempty_list -> 'a nonempty_list

val to_bytes :  ('a -> bytes) -> 'a nonempty_list -> bytes