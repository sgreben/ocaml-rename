module M = struct
    let value_in_M = 123
end

let f x = x + M.value_in_M
