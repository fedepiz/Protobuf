use "Comparable.sml";

signature MAP =
sig
	type key
	type 'a map
	exception ItemNotFoundException
	val empty : 'a map
	val add : key * 'a -> 'a map -> 'a map
	val remove : key -> 'a map -> 'a map
	val lookup : key -> 'a map -> 'a option
	val get : key -> 'a map -> 'a
	val transform : ('a -> 'b) -> 'a map -> 'b map
	val foldl : ((key * 'a) * 'b -> 'b) -> 'b -> 'a map -> 'b
end

functor Map(C:COMPARABLE):MAP =
struct
	exception ItemNotFoundException
	type key = C.t
	type 'a map = (key * 'a) list
	val f = C.compare
	val empty = []:(key * 'a) list
	fun lookup _ [] = NONE
	  | lookup k ((k',v)::xs) = case f(k,k') of
									EQUAL => SOME(v)
								|	_ => lookup k xs
	fun get k mp = case lookup k mp of
						SOME(x) => x
					  | NONE => raise ItemNotFoundException
	fun add x [] = [x]
	  | add (k,x) ((k',x')::xs) = case f(k,k') of
								EQUAL => (k,x)::xs
							|	_ => (k',x')::(add (k,x) xs)
	fun remove k [] = []
	  | remove k ((k',v)::xs) = case f(k,k') of
								EQUAL => xs
							| _ => (k',v)::(remove k xs)
	fun transform _ [] = []
	  | transform f ((k,v)::xs) = (k,f v)::(transform f xs)
	  
	fun foldl f v x = List.foldl f v x
end

structure IntMap = Map(type t = int val compare = Int.compare)
structure StringMap = Map(type t = string val compare = String.compare)