(*use "Bits.sml";*)
structure ZigZag =
struct
	open Bits
	fun encodeZigZag32 n = xorb(<<(n,0w1),~>>(n,0w31))
	fun encodeZigZag64 n = xorb(<<(n,0w1),~>>(n,0w64))
	fun decodeZigZag n = xorb(~>>(n,0w1),~(andb(n,1)))
end :
sig
	val encodeZigZag32 : int -> int
	val encodeZigZag64 : int -> int
	val decodeZigZag : int -> int
end