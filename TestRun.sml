use "Load.sml";
exception TestFailedException;
val source = ("message Test = { required int32 a = 1;\n required string b = 2; required double c = 3; }")
val msgDef = case ProtoParser.parseMessages(source) of
				Parser.Success(msg) => msg
			|	Parser.Failure _ => raise TestFailedException
val aLens = ProtoLens.intLens "a"
val bLens = ProtoLens.stringLens "b"
val cLens = ProtoLens.realLens "c"
open Lens;
val msg = chain3(setM aLens 1,setM bLens "testString", setM cLens 3.4) (Proto.emptyMessage "test1")