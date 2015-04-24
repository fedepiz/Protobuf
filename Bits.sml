structure Bits =
struct
	fun ~>>(x,n) = IntInf.toInt(IntInf.~>>(IntInf.fromInt x,n))
	fun <<(x,n) = IntInf.toInt(IntInf.<<((IntInf.fromInt x),n))
	fun orb(x,y) = IntInf.toInt(IntInf.orb(IntInf.fromInt x,IntInf.fromInt y))
	fun andb(x,y) = IntInf.toInt(IntInf.andb(IntInf.fromInt x,IntInf.fromInt y))
	fun xorb(x,y) = IntInf.toInt(IntInf.xorb(IntInf.fromInt x,IntInf.fromInt y))
	fun fromTemplate(0,acc,_)= rev acc
	  | fromTemplate(n,acc,x) = fromTemplate(n-1,Word8.fromInt(x)::acc,~>>(x,0w8))
	fun fromInt32(x) = fromTemplate(4,[],x)
	fun fromInt64(x) = fromTemplate(8,[],x)
	fun toInt xs = 
		let fun inner([],v) = v
		      | inner((x::xs),v) = inner(xs,orb(<<(v,0w8),Word8.toInt(x)))
		in
			inner(rev xs,0)
		end
	fun toIntOption xs = SOME(toInt xs) handle _ => NONE
end :
sig
	val ~>> : int * word -> int
	val << : int * word -> int
	val andb : int * int -> int
	val orb : int * int -> int
	val xorb : int * int -> int
	val fromInt32 : int -> Word8.word list
	val fromInt64 : int -> Word8.word list
	val toInt : Word8.word list -> int
	val toIntOption : Word8.word list -> int option
end