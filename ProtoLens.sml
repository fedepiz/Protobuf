(*use "Proto.sml";
use "Lens.sml";*)

structure ProtoLens = 
struct
	open Proto;
	open ProtoAccess;
	open Lens
	type 'a messageGetter = string * protoMessage -> 'a
	type 'a messageSetter = string * 'a * Proto.protoMessage -> protoMessage
	type 'a protoLens = (protoMessage,'a) lens
	fun makeFieldLens(f,g) n = lens(fn x => f(n,x),fn (v,x) => g(n,v,x))
	val intLens = makeFieldLens(getIntValue,setIntValue)
	val realLens = makeFieldLens(getRealValue,setRealValue)
	val stringLens = makeFieldLens(getStringValue,setStringValue)
	val boolLens = makeFieldLens(getBoolValue,setBoolValue)
	val bytesLens = makeFieldLens(getBytesValue,setBytesValue)
	val protoMessageLens = makeFieldLens(getProtoMessageValue,setProtoMessageValue)
end :
sig
	type 'a messageGetter = string * Proto.protoMessage -> 'a
	type 'a messageSetter = string * 'a * Proto.protoMessage -> Proto.protoMessage
	type 'a protoLens = (Proto.protoMessage,'a) Lens.lens
	val makeFieldLens : 'a messageGetter * 'a messageSetter -> string -> (Proto.protoMessage,'a) Lens.lens
	val intLens : string -> int protoLens
	val realLens : string -> real protoLens
	val stringLens : string -> string protoLens
	val boolLens : string -> bool protoLens
	val bytesLens : string -> (Word8.word list) protoLens
	val protoMessageLens : string -> Proto.protoMessage protoLens
end