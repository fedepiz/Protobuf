(*use "Bits.sml";
use "Streams.sml";*)
signature VARINT = 
sig
	exception VarintFormatException
	val encodeVarint : int -> Word8.word list
	val decodeVarint : Word8.word list -> int
	val decodeVarintOption : Word8.word list -> int option
	val decodeFromStream : Word8.word ListInstream.instream -> int * Word8.word ListInstream.instream 
end

structure Varint:VARINT =
struct
	open ListInstream
	exception VarintFormatException
	fun zeroMSB x = Word8.andb(Word8.fromInt(x),0w127)
	fun oneMSB x = Word8.orb(Word8.andb(Word8.fromInt(x),0w127),0w128)
	fun iZeroMSB x = Bits.andb(x,127)
	fun iOneMSB x = Bits.orb(x,128)
	fun iShiftR(x,y) = Bits.<<(x,Word.fromInt y)
	fun isMSBOne x = Word8.>(Word8.andb(0w128, x),0w0)
	
	fun encodeVarint x = let fun inner x =
		if (x <= 127)
		then [zeroMSB x]
		else (oneMSB x)::(encodeVarint (Bits.~>>(x,0w7)))
		in 
			 inner x
		end
		
	fun decodeVarint [] = raise VarintFormatException
	  | decodeVarint ls = 
				let fun decodeVarintF [] acc _ = acc
				      | decodeVarintF (x::xs) acc i = 
							let val newAcc = Bits.orb(acc,iShiftR(iZeroMSB(Word8.toInt x),7*i)) in
								decodeVarintF xs newAcc (i+1)
							end 
			in 
				decodeVarintF (rev ls) 0 0
			end
	fun decodeVarintOption ls = SOME(decodeVarint ls) 
							    handle VarintFormatException => NONE
	fun decodeFromStream stream = 
		let val (ls,s1) = readWhile(isMSBOne,stream)
			val (last,s2) = read(s1) in
				(decodeVarint (rev(ls @ [last])),s2)
		end
end