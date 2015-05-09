use "Load.sml";
exception TestFailedException;
open Lens;

val aLens = ProtoLens.intLens "a"
val bLens = ProtoLens.stringLens "b"
val cLens = ProtoLens.protoMessageLens "c"

val sourceGoogle = "message Test1 = { required int32 a = 1; } message Test3 = { required Test1 c = 3; }"
val result = ProtoParser.parseMessages(sourceGoogle)
val msgDefG = case result of
				Parser.Success(x) => hd x
			|	Parser.Failure _ => raise TestFailedException
val msgG = set aLens (150,Proto.bareMessage msgDefG)
val enG = ProtoEncoding.encodeMessageToStream(msgG)
val decG = ProtoEncoding.decodeMessageFromStream(msgDefG,enG)

val sourceGoogle2 = "message Test2 = { required string b = 2; }"
val result2 = ProtoParser.parseMessages(sourceGoogle2)
val msgDefG2 = case result2 of
				Parser.Success(x) => hd x
			|	Parser.Failure _ => raise TestFailedException
val msgG2 = set bLens ("testing",Proto.bareMessage msgDefG2)
val enG2 = ProtoEncoding.encodeMessageToStream(msgG2)
val decG2 = ProtoEncoding.decodeMessageFromStream(msgDefG2,enG2)

val msgDefG3 = case result of
					Parser.Success(x) => hd(tl x)
				|	Parser.Failure _ => raise TestFailedException
val msgG3 = set cLens (msgG,Proto.bareMessage msgDefG3)
val enG3 = ProtoEncoding.encodeMessageToStream(msgG3)
val decG3 = ProtoEncoding.decodeMessageFromStream(msgDefG3,enG3)