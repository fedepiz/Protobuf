signature COMPARABLE =
sig
	type t;
	val compare : t * t -> order
end