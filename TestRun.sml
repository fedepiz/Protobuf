use "Load.sml";
exception TestFailedException;
open Lens;
infix +>
val sourceGoogle = "message Test1 = { required int32 a = 1; } message Test3 = { required Test1 c = 3; }"
val result = ProtoParser.parseMessages(sourceGoogle)
val msgDef = case result of
				Parser.Success(x) => hd x
			|	Parser.Failure _ => raise TestFailedException
val a = ProtoLens.intLens "a"
val msg = set a (150,Proto.bareMessage msgDef)
val en = ProtoEncoding.encodeMessageToStream(msg)
val dec = ProtoEncoding.decodeMessageFromStream(msgDef,en)

val sourceGoogle2 = "message Test2 = { required string b = 2; }"
val result2 = ProtoParser.parseMessages(sourceGoogle2)
val msgDef2 = case result2 of
				Parser.Success(x) => hd x
			|	Parser.Failure _ => raise TestFailedException
val b = ProtoLens.stringLens "b"
val msg2 = set b ("testing",Proto.bareMessage msgDef2)
val en2 = ProtoEncoding.encodeMessageToStream(msg2)
val dec2 = ProtoEncoding.decodeMessageFromStream(msgDef2,en2)

val msgDef3 = case result of
					Parser.Success(x) => hd(tl x)
				|	Parser.Failure _ => raise TestFailedException
val c = ProtoLens.protoMessageLens "c"
val msg3 = set c (msg,Proto.bareMessage msgDef3)
val en3 = ProtoEncoding.encodeMessageToStream(msg3)
val dec3 = ProtoEncoding.decodeMessageFromStream(msgDef3,en3)

