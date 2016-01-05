let table : (Types.signature_item * Types.signature_item) list ref = ref []

let clear () = table := []

let add left right = table := (left,right) :: !table

let to_alist () = !table