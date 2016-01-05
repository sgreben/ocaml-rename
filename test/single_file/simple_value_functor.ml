module type S = sig
	val value_in_S : int
end

module F(S:S) = struct
	module S = S
	let value_in_F = S.value_in_S * 2
end

module F1 = F(struct let value_in_S = 123 end)

let x = F1.S.value_in_S

let y = F1.value_in_F
