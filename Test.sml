use "Load.sml";
val msgDef = Proto.messageDef(Proto.sampleMessage2)
val msg = Proto.sampleMessage2
val x = ProtoLensGen.generateMessageLensCode(msgDef)
val y = ProtoLensGen.generateMessageLensTypeCode(msgDef,[])
val f = ProtoLens.intLens "first"