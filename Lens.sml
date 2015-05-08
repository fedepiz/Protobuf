structure Lens  =
struct
	datatype ('a,'b) lens = Lens of ('a -> 'b) * ('b * 'a -> 'a)
	fun lens x = Lens x
	fun getter (Lens(x,_)) = x
	fun setter (Lens(_,x)) = x
	fun get (Lens(g,_)) x = g(x)
	fun set (Lens(_,s)) x = s(x)
	fun modify f (Lens(g,s)) x = s(f(g(x)),x)
	fun compose (a_b,b_c) = Lens (
		fn a => get b_c(get a_b a),
		fn (c,a) => set a_b (set b_c (c,get a_b a),a))

	val monoLens = Lens ( fn x => x, fn (x,_) => x)
	val firstLens = Lens (fn (x,_) => x, fn(x,(_,y)) => (x,y))
	val secondLens = Lens (fn (_,y) => y, fn(y,(x,_)) => (x,y))
	val headLens = Lens (fn ls => hd ls,
						 fn (x,ls) => x::(tl ls))
	val tailLens = Lens(fn ls => tl ls,
						fn (x,ls) => (hd ls)::x)
	fun +>(x,y) = compose(x,y)
	fun <+(x,y) = compose(y,x)
	fun getM l1 = fn x => get l1 x
	fun setM l1 v = fn x => set l1 (v,x)
	fun chain(f,g) = fn x => g(f(x))
	fun chain3(f,g,h) = fn x => h(g(f(x)))
	fun chain4(f,g,h,i) = fn x => i(h(g(f(x))))
end :
sig
	type ('a,'b) lens
	val lens : (('a -> 'b) * ('b * 'a -> 'a)) -> ('a,'b) lens
	val getter : ('a,'b) lens -> ('a -> 'b)
	val setter : ('a,'b) lens -> ('b * 'a -> 'a)
	val get : ('a,'b) lens -> 'a -> 'b
	val set : ('a,'b) lens -> 'b * 'a -> 'a
	val modify : ('b -> 'b) -> ('a,'b) lens -> 'a -> 'a
	val compose : ('a,'b) lens * ('b,'c) lens -> ('a,'c) lens
	val +> : ('a,'b) lens * ('b,'c) lens -> ('a,'c) lens
	val <+ : ('b,'c) lens * ('a,'b) lens -> ('a,'c) lens
	val getM : ('a,'b) lens -> ('a -> 'b)
	val setM : ('a,'b) lens -> 'b -> ('a -> 'a)
	val chain : ('a -> 'b) * ('b -> 'c) -> ('a -> 'c)
	val chain3 : ('a -> 'b) * ('b -> 'c) * ('c -> 'd)-> ('a -> 'd)
	val chain4 : ('a -> 'b) * ('b -> 'c) * ('c -> 'd) * ('d -> 'e)-> ('a -> 'e)
	val monoLens : ('a,'a) lens
	val firstLens : ('a * 'b,'a) lens
	val secondLens : ('a * 'b,'b) lens
	val headLens : ('a list,'a) lens
	val tailLens : ('a list, 'a list) lens
end