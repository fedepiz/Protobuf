structure StringManipulation =
struct
	fun upperFirst str = 
		case String.explode str of 
			[] => ""
		|	x::xs => String.implode ((Char.toUpper x)::xs)
	fun numToLetters x = 
		case x of 
				#"0" => "Zero"
			|	#"1" => "One"
			|	#"2" => "Two"
			|	#"3" => "Three"
			|	#"4" => "Four"
			|	#"5" => "Five"
			|	#"6" => "Six"
			|	#"7" => "Seven"
			|	#"8" => "Eight"
			|	#"9" => "Nine"
			|	y => Char.toString y
	fun replaceNumbersWithLetters str =
		String.concat(map numToLetters (String.explode str))
end