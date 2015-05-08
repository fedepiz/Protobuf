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
val encodedStream = WireEncoding.encodeWireMessage(ProtoEncoding.encodeProtoMessage(msg))
val decodedMessage = ProtoEncoding.decodeProtoMessage(msgDef,WireEncoding.decodeWireMessage(encodedStream))