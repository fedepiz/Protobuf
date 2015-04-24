structure Wire = 
struct
	exception NotImplemented
	datatype wireValue = Varint of int
					| I32 of int
					| I64 of int
					| Bytes of Word8.word list
					| WireMessage of (int * wireValue) list
	type wireField = int * wireValue
	type wireMessage = wireField list
	fun wireKey(k,_) = k
	fun wireVal(_,v) = v
end : 
sig
	datatype wireValue = Varint of int
					| I32 of int
					| I64 of int
					| Bytes of Word8.word list
					| WireMessage of (int * wireValue) list
	type wireField = int * wireValue
	type wireMessage = wireField list
	val wireKey : wireField -> int
	val wireVal : wireField -> wireValue
end