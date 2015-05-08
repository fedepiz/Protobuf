structure ProtoParser =
struct
	open Parser
	open Proto
	exception UndefinedMessage of string
	fun toType s =
		case s of 
			"int32" => TInt32
		|	"int64" => TInt64
		|	"uint32" => TUInt32
		|	"uint64" => TUInt64
		|   "sint32" => TSInt32
		|	"sint64" => TSInt64
		|	"fixed32" => TFixed32
		|	"fixed64" => TFixed64
		|	"sfixed32" => TSFixed32
		|	"sfixed64" => TSFixed64
		|	"float" =>  TFloat
		|	"double" => TDouble
		| 	"string" => TString
		|	"bool"	=> TBool
		|	"bytes" => TBytes
		|	x => TProtoMessage (MessageDef(x,[],[]))
	val parseType = lift toType identifier
	val parseKey = wrap(
					ws(symbol #"="),
					ws(integer),
					ws(symbol #";"))
	val parseKeyVal = seq(ws(identifier),parseKey)
	val parseOptionType = seq(ws(word),ws(parseType))
	val parseField = lift (fn((opt,t),(name,key)) => FieldDef(Required,t,name,key)) 
						   (seq(parseOptionType,parseKeyVal))
	val parseFields = many(ws(parseField))
	val messageBody = wrap(symbol #"{",ws(parseFields),symbol #"}")
	val messageParser = precede(ws(keyword "message"),seq(ws(identifier),precede(ws(symbol #"="),messageBody)))
	val message = lift (fn (name,fields) => MessageDef(name,[],fields)) messageParser
	val messages = many(ws(message))
	fun replaceWithProperMessage(message,messages) =
		case List.find (fn x => messageDefName(message) = messageDefName(x)) messages of
			SOME(x) => x
		|	NONE => raise UndefinedMessage(messageDefName(message))
	fun fixField messages field =
		let val FieldDef(opt,t,name,key) = field in
			case t of
				TProtoMessage msg => let val replacement = TProtoMessage(replaceWithProperMessage(msg,messages)) in
																FieldDef(opt,replacement,name,key)
															end
			|	_ => field
		end
	fun fixMessage m(MessageDef(n,opt,fields)) = MessageDef(n,opt,map (fixField m) fields)
	fun fixMessages m ms = map (fixMessage m) ms
	fun parseMessages str = case parse(messages,str) of
								Failure x => Failure x
							|	Success(x) => Success(fixMessages x x) 
									handle UndefinedMessage(m) => Failure(String.concat["Undefined message",m])
	val test = parseMessages("message Bla = { required string a = 1;} message Test = { required Bla t = 1;}")
end :
sig
	val parseMessages : string -> (Proto.protoMessageDef list,string) Parser.result
end

 