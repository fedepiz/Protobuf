signature IN_STREAM =
sig
	type 'a instream
	type ('a,'b) consumer = 'a instream -> ('b * 'a instream)
	exception EndOfStreamException
	val fromList : 'a list -> 'a instream
	val read : 'a instream -> 'a * 'a instream
	val readN : int * 'a instream -> ('a list) * 'a instream
	val readOption : 'a instream -> ('a * 'a instream) option
	val readNOption :int * 'a instream -> (('a list) * 'a instream) option
	val readWhile : ('a -> bool) * 'a instream -> 'a list * 'a instream
	val isEmpty : 'a instream -> bool
	val process : ('a,'b) consumer * 'a instream -> 'b list
end

signature OUT_STREAM =
sig
	type 'a outstream
	exception StreamOverflowException
	val toList : 'a outstream -> 'a list
	val write : 'a * 'a outstream -> 'a outstream
	val writeList : 'a list * 'a outstream -> 'a outstream
	val empty : 'a outstream
end

signature STREAMS =
sig
	type 'a instream
	type 'a outstream
	val toOutstream : 'a instream -> 'a outstream
	val toInstream : 'a outstream -> 'a instream
	val copyTo : 'a instream -> 'a outstream -> 'a outstream
end

structure ListInstream : IN_STREAM =
struct
	type 'a instream = 'a list
	type ('a,'b) consumer = 'a instream -> ('b * 'a instream)
	exception EndOfStreamException
	fun fromList x = x
	fun read [] = raise EndOfStreamException
	  | read (x::xs) = (x,xs)
	fun readOption x = SOME(read x) handle EndOfStreamException => NONE
	fun readNInner (ls,0,stream) = (rev ls,stream)
	  | readNInner (_,_,[]) = raise EndOfStreamException
	  | readNInner (ls,n,x::rest) = readNInner(x::ls,n-1,rest)
	fun readN (n,stream) = readNInner ([],n,stream)
	fun readNOption x = SOME(readN x) handle EndOfStreamException => NONE
	fun readWhileInner(ls,_,[]) = (rev ls,[])
	  | readWhileInner(ls,p,x::xs) = case p(x) of
										true => readWhileInner(x::ls,p,xs)
									|   false => (rev ls,x::xs)
	fun readWhile (p,s) = readWhileInner([],p,s)
	fun isEmpty [] = true
	  | isEmpty _ = false
	fun processAcc(acc,f,[]) = rev acc
	  | processAcc(acc,f, str) = 
		let val (x,xs) = f(str) in
			processAcc(x::acc,f,xs)
		end
	fun process(f,str) = processAcc([],f,str)
end

structure ListOutstream : OUT_STREAM =
struct
	type 'a outstream = 'a list
	exception StreamOverflowException
	val empty = []
	fun write (x,xs) = x::xs
	fun writeList ([],xs) = xs
	  | writeList (x::xs, rest) = writeList(xs,write(x,rest))
	fun toList x = rev x
end

functor Streams(structure InS:IN_STREAM
				structure OutS:OUT_STREAM):STREAMS =
struct
	type 'a instream = 'a InS.instream
	type 'a outstream = 'a OutS.outstream
	fun toInstream x = InS.fromList(OutS.toList x)
	fun copyTo source dest = let val (x,xs) = InS.read source in
								copyTo xs (OutS.write(x,dest))
							end handle EndOfStreamException => dest
	fun toOutstream x = copyTo x (OutS.empty)
end

structure ListStreams = Streams(structure InS = ListInstream 
								structure OutS = ListOutstream);