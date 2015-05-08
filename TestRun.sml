use "Load.sml";
exception TestFailedException;
val source = ("message Test = { required int32 a = 1;\n required string b = 2; required double c = 3; }")
val msgDef = case ProtoParser.parseMessages(source) of
				Parser.Success(msg) => hd msg
			|	Parser.Failure _ => raise TestFailedException
open Lens;
val bare = Proto.bareMessage msgDef
val aLens = ProtoLens.intLens "a"
val bLens = ProtoLens.stringLens "b"
val cLens = ProtoLens.realLens "c"
val msg = chain3(setM aLens 1,setM bLens "testString", setM cLens 3.4) bare
fun encodedStream x = WireEncoding.encodeWireMessage(ProtoEncoding.encodeProtoMessage(x))
fun decodedMessage (def,x) = 
	ProtoEncoding.decodeProtoMessage(def,WireEncoding.decodeWireMessage(x))

val source2 = "message Test1 = { required int32 a = 1; }"
val result2 = ProtoParser.parseMessages(source2)
val msgDef2 = case result2 of
				Parser.Success(x) => hd x
			|	Parser.Failure _ => raise TestFailedException
val msg2 = set aLens (150,Proto.bareMessage msgDef2)
val en2 = encodedStream msg2
val dec2 = decodedMessage (msgDef2,en2)