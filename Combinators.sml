structure Iterators =
struct
	fun iterateOpt f x = case f(x) of
						SOME(y) => iterateOpt f y
					|   NONE => x
					
	fun iterateN f x n = if (n <= 0)
						 then x
						 else iterateN f (f x) (n - 1)
						 
	fun iterateWhile p f x = case p(x) of
								true => iterateWhile p f (f x)
							|   false => x
				
	fun iterateUntil p f x = iterateWhile (fn x => not (p x)) f x
	
	fun iterateEx f x = iterateEx f (f x) handle _ => x
end

structure Combinators =
struct
	fun fst (f,(x,y)) = (f x,y)
	fun snd (f,(x,y)) = (x,f y)
	fun >> (x,y) = fn t => y(x(t))
	infix >>
end