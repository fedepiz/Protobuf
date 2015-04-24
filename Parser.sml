structure Parser =
struct
	type sourceStream = (char list * char list)
	fun makeSourceStream str = ([],String.explode str)
	fun current (x,y) = hd y
	fun next (x,y) = ((hd y)::x,tl y)
	fun prev (x,y) = (tl x,(hd x)::y)
	fun isOver (_,[]) = true
	  | isOver _ = false
	type position = (int * int)
	fun lineno (x,_) = x
	fun colno (_,x) =  x
	fun incLineno (x,y) = (x+1,y)
	fun incColNo (x,y) = (x,y+1)
	fun resetColNo (x,_) = (x,0)
	datatype parserState = PS of sourceStream * position
	datatype ('a,'b) result = Success of 'a  | Failure of 'b
	type 'a parser = parserState -> ('a * parserState,string)result
	fun id x = x
	fun onSuccess f r = case r of
				Success x => Success (f x)
			|	Failure x => Failure x
	fun onFail f r = case r of
				Success x => Success x
			|	Failure x => Failure (f x)
	fun makeParserState str = PS(makeSourceStream str,(0,0))
	fun withLocation (msg,PS(_,(lineno,colno))) = 
		String.concat [msg," at line: ",Int.toString lineno]
	fun parse (p1,str) = let val state = (makeParserState str) 
							 val result = p1 state in
							case result of
								Success(x,_) => Success(x)
							|	Failure x => Failure(withLocation(x,state))
						 end
	fun bind (p1,f) state =
		case p1 state of
			Failure x => Failure x
		|	Success (x,rest) => f(x) rest
	fun always state = Success((),state)
	fun never _ = Failure("Failure parsed")
	fun alt (p1,p2) state = 
		case p1 state of
			Success x => Success x
		|	Failure y => (case p2 state of
							Success x => Success x
						  |	Failure z => Failure (String.concat[y, " or ", z]))
	fun seq (p1,p2) state =
		case p1 state of
			Failure x => Failure x
		|	Success (x1,rest) => (case p2 rest of
									Failure x => Failure x
								  |	Success (x2,final) => Success ((x1,x2),final))
	fun lift f p1 state = 
		case p1 state of
			Success (x,rest) => Success (f x,rest)
		|	Failure x => Failure x
	fun precede (p1,p2) = lift #2 (seq(p1,p2))
	fun follow (p1,p2) = lift #1 (seq(p1,p2))
	fun wrap(p1,p2,p3) = follow(precede(p1,p2),p3)
	fun separate(p1,p2,p3) = lift (fn ((x,y),z) => (x,z)) (seq(seq(p1,p2),p3))
	fun oneOf parsers state = 
		let fun inner([],errS,_) = Failure errS
		     |  inner(p::ps,errS,state) = case p state of
											Success x => Success x 
										  | Failure x => inner(ps,String.concat[errS,x,"\n"],state)
				in
					inner(parsers,"",state)
				end
	fun until(test,p) state = 
		let fun inner(test,p,accum) state = 
			case test state of 
				Success _ => Success(rev accum,state)
			|	Failure _ => (case p state of
								Failure x => Failure x
							|	Success (x,rest) => inner(test,p,x::accum) rest) 
			in
				inner(test,p,[]) state
			end
	fun many1 p state = 
		let fun inner (p,state,accum) =
			case p state of
				Failure x => (case accum of 
								[] => Failure x
							|	_ => Success(rev accum,state))
			|	Success (x,rest) => inner(p,rest,x::accum) in
				inner(p,state,[])
			end
	fun many p state = 
		case (many1 p) state of 
			Success x => Success x
		|	Failure _ => Success([],state)
	fun maybe p state =
			case p state of 
				Success (x,rest) => Success(SOME(x),rest)
			|	Failure x => Success(NONE,state)
	fun sepBy(p1,p2) state = 
		let fun inner(p1,p2,accum) state = 
			case p1 state of
				Failure x => Failure x
			|	Success (x,rest) => (case p2 rest of
										Failure _ => Success(rev (x::accum),rest)
									|	Success (_,restRest) => inner(p1,p2,x::accum) restRest)
 		in
			inner(p1,p2,[]) state
		end
	fun transform f p state = 
		case p state of
			Failure x => Failure x
		|	Success (x,rest) => onSuccess (fn t => (t,rest)) (f x)
	fun modifyPosition(c,(line,col)) = 
		case c of
			#"\n" => (line + 1,0)
		|	_ => (line,col + 1)
	fun anySymbol (PS(ss,pos)) = case isOver ss of
								false => let val c = current ss in
											Success(c,PS(next ss,modifyPosition(c,pos)))
										end
							|	true => Failure "End of stream"
	fun pSymbol (p,msg) state = 
		case anySymbol state of
			Failure _ => Failure msg
		|	Success(x,nstate) => if p(x)
								 then Success(x,nstate)
								 else Failure msg
	fun symbol c = pSymbol(fn x => x = c,String.concat[Char.toString c," expected"])
	val alpha = pSymbol(Char.isAlpha,"letter expected")
	val alphaNum = pSymbol(Char.isAlphaNum,"letter or digit expected")
	val digit = pSymbol(Char.isDigit,"digit expected")
	val whitespace = pSymbol(Char.isSpace,"whitespace expected")
	val stringLiteral = let val quote = symbol #"\"" 
							val p = wrap(quote,until(quote,anySymbol),quote) in
								lift (String.implode) p
						end
	val word = lift String.implode (many1 alpha)
	val number = lift String.implode (many1 digit)
	val integer = 
		let fun f x = (case Int.fromString(x) of 
						SOME(x) => Success(x)
					|	NONE => Failure "Cannot parse int") in
							transform f number
							end
	val dottedNumber = 
		let fun f((x,y),z) = String.concat([x,Char.toString y,z]) in
			lift f (seq(seq(number,symbol #"."),number))
		end
	
	val floating = 
		let fun f x = (case Real.fromString(x) of
						SOME(x) => Success(x)
					|	NONE  => Failure "Cannot parse real") in
						transform f dottedNumber
					end
	fun ws p1 = wrap(many whitespace,p1,many whitespace)
end :
sig
	type parserState
	datatype ('a,'b) result = Success of 'a | Failure of 'b
	type 'a parser = parserState -> ('a * parserState,string)result
	val parse : ('a parser * string) -> ('a, string) result
	val bind : 'a parser * ('a -> 'b parser) -> 'b parser
	val always : unit parser
	val never : 'a parser
	val anySymbol : char parser
	val pSymbol : (char -> bool) * string -> char parser
	val symbol : char -> char parser
	val alt : 'a parser * 'a parser -> 'a parser
	val seq : 'a parser * 'b parser -> ('a * 'b) parser
	val lift :('a -> 'b) -> 'a parser -> 'b parser
	val precede : 'a parser * 'b parser -> 'b parser
	val follow : 'a parser * 'b parser -> 'a parser
	val wrap : 'a parser * 'b parser * 'c parser -> 'b parser
	val oneOf : ('a parser) list -> 'a parser
	val until : 'a parser * 'b parser -> ('b list) parser
	val many1 : 'a parser -> ('a list)parser
	val many : 'a parser -> ('a list) parser
	val sepBy : 'a parser * 'b parser -> ('a list) parser
	val transform : ('a -> ('b,string) result) -> 'a parser -> 'b parser 
	val separate : 'a parser * 'b parser * 'c parser -> ('a * 'c) parser
	val alpha : char parser
	val alphaNum : char parser 
	val digit : char parser
	val whitespace : char parser
	val stringLiteral : string parser
	val word : string parser
	val number : string parser
	val integer : int parser
	val dottedNumber : string parser
	val floating : real parser
	val ws : 'a parser -> 'a parser
	val maybe : 'a parser -> ('a option) parser
end

fun test s = Parser.parse(Parser.sepBy(Parser.integer,Parser.ws(Parser.symbol #",")),s)