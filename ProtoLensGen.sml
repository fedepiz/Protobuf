(*use "ProtoLens.sml";
use "StringManipulation.sml";*)
structure ProtoLensGen = 
struct
	open Proto;
	fun nativeNameFromType t = 
		case t of 
			TInt32 => "int"
		|	TInt64 => "int"
		|	TUInt32 => "int"
		|	TUInt64 => "int"
		|	TFixed32 => "int"
		|	TFixed64 => "int"
		|	TSInt32 => "int"
		|	TSInt64 => "int"
		|	TSFixed32 => "int"
		|	TSFixed64 => "int"
		|	TFloat => "real"
		|	TDouble => "real"
		|	TBool	=> "bool"
		|	TString => "string"
		|	TBytes => "bytes"
		|	TProtoMessage _ => "protoMessage"
	fun lensNameFromType x = String.concat ["ProtoLens.",nativeNameFromType x,"Lens"]
	datatype capitalization = NO | UPPERFIRST
	fun cleanName (upper,x) = 
		let val s = StringManipulation.replaceNumbersWithLetters x in
			case upper of
				UPPERFIRST => StringManipulation.upperFirst(s)
			|	NO => s
		end
	fun generateLeftSide (msgName,fieldName) = ["val ",cleanName (NO,msgName),cleanName (UPPERFIRST,fieldName)]
	fun generateValueRightSide (fieldType,fieldName) = [" = ", lensNameFromType fieldType, " \"",fieldName,"\"\n"]
	fun generateTypeRightSide'(fieldType,specialNames) = 
		case fieldType of
			TProtoMessage msg => (case List.find (fn x => Proto.messageDefName(msg) = x) specialNames of
									NONE => "protoMessage"
								|	SOME(n) => n)
		|	t => nativeNameFromType t
	fun generateTypeRightSide (fieldType,specialNames) = 
		[" : ", generateTypeRightSide'(fieldType,specialNames)," ", "ProtoLens.protoLens\n"]
	fun generateFieldLensCode msgName (FieldDef(_,fieldType,fieldName,_)) =
		String.concat (generateLeftSide(msgName,fieldName) @ generateValueRightSide(fieldType,fieldName))
	fun generateFieldLensTypeCode (msgName,specialNames) (FieldDef(_,fieldType,fieldName,_)) =
		String.concat(generateLeftSide(msgName,fieldName) @ generateTypeRightSide(fieldType,specialNames))
	fun generateMessageLensCode (MessageDef(name,_,fields)) = 
		String.concat(map (generateFieldLensCode name) fields)
	fun generateMessageLensTypeCode (MessageDef(name,_,fields),ls) =
		String.concat(map (generateFieldLensTypeCode (name,ls)) fields)
	fun packUpStructure(structName,definedTypes,structBody,structSig,isOpaque) = 
		String.concat["structure ", structName, " = \n",
					"struct\n",
					definedTypes,
					structBody,
					"\nend", if isOpaque then ":>" else ":",
					"sig\n",
					definedTypes,
					structSig,
					"\nend"]
end :
sig
	val generateFieldLensCode : string -> Proto.protoFieldDef -> string
	val generateFieldLensTypeCode : string * string list -> Proto.protoFieldDef -> string
	val generateMessageLensCode : Proto.protoMessageDef -> string
	val generateMessageLensTypeCode : Proto.protoMessageDef * string list -> string
end