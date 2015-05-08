(*use "Bits.sml";
use "Streams.sml";
use "Varint.sml";
use "Wire.sml";*)
structure WireEncoding =
struct
	open Wire
	open ListInstream
	exception NotImplementedException
	exception UnknownFieldType of int
	fun vectorToList vec = 
		let fun inner (vec,n) = if (Word8Vector.length(vec) = n)
								then []
								else Word8Vector.sub(vec,n)::(inner(vec,n+1))
				in
					inner(vec,0)
				end
	val tagVarint = 0
	val tagI64 = 1
	val tagLenDelimited = 2
	val tagI32 = 5
	fun makeTagW(k,t) = Word8.orb(Word8.<<(k,0w3),t)
	fun makeTag(key,typ) = makeTagW(Word8.fromInt key,Word8.fromInt typ)
	fun encodeWireField(k,Varint(x)) = makeTag(k,tagVarint)::(Varint.encodeVarint(x))
	  | encodeWireField(k,I32(x)) = makeTag(k,tagI32)::Bits.fromInt32(x)
	  | encodeWireField(k,I64(x)) = makeTag(k,tagI64)::Bits.fromInt64(x)
	  | encodeWireField(k,Bytes(x)) = 
	     makeTag(k,tagLenDelimited)::(Varint.encodeVarint(List.length(x))) @ x
	  | encodeWireField(k,WireMessage(x)) = encodeWireField(k,Bytes(encodeWireMessage x))
	and encodeWireMessage mp = 
		let fun inner((k,v),acc) = acc @ encodeWireField(k,v) in
			(foldl inner [] mp)
		end
	fun decodeWireVarint(stream) = 
		let val (x,rest) = Varint.decodeFromStream(stream) in
			(Varint(x),rest) end
	fun decodeWireInt32(stream) = 
		let val (ls,rest) = readN(4,stream) in
			(I32(Bits.toInt ls),rest) end
	fun decodeWireInt64(stream) = 
		let val (ls,rest) = readN(8,stream) in
			(I64(Bits.toInt ls),rest) end
	fun decodeWireBytes(stream) = 
		let val (blen,s1) = read stream 
			val (body,rest) = readN(Word8.toInt blen,s1) in
				(Bytes(body),rest)
			end
	fun tagToNumAndType n = 
		let val fieldType = Word8.toInt(Word8.andb(n,0w7))
		    val fieldNum =  Word8.toInt(Word8.~>>(Word8.andb(n,0w248),0w3)) in
				(fieldNum,fieldType)
			end
	fun decodeWireField stream = 
		let val (tag,rest) = read(stream)
			val (fieldNum,fieldType) = tagToNumAndType(tag)
			val (v,rest2) = (case fieldType of
							0 => decodeWireVarint(rest)
						  | 5 => decodeWireInt32(rest)
						  | 1 => decodeWireInt64(rest)
						  | 2 => decodeWireBytes(rest)
						  | x => raise UnknownFieldType(x)) in
								((fieldNum,v),rest2) end
	fun decodeWireMessage stream = process(decodeWireField,stream)
end :
sig
	exception UnknownFieldType of int
	val encodeWireField : int * Wire.wireValue -> Word8.word list
	val encodeWireMessage : Wire.wireMessage -> Word8.word list
	val decodeWireField : (Word8.word, Wire.wireField) ListInstream.consumer
	val decodeWireMessage : Word8.word ListInstream.instream -> Wire.wireMessage
end