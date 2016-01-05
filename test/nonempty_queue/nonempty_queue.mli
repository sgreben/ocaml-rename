module L = Nonempty_list

type 'a t = {
  neq_enqueue : 'a L.nonempty_list option;
  neq_dequeue : 'a L.nonempty_list;
}

type 'a dequeue_result = { dr_element : 'a; dr_queue : 'a t option; }

val enqueue : 'a t -> 'a -> 'a t

val dequeue : 'a t -> 'a dequeue_result
