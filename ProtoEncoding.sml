(*use "Proto.sml";
use "Wire.sml";
use "ZigZag.sml";
use "Bits.sml";
*)
structure ProtoEncoding =
struct
	exception NotImplementedException
	exception DecodingFieldException of Proto.protoFieldDef * Wire.wireValue * Proto.protoType
	exception EndOfWireMessageException of Proto.protoFieldDef list
	exception FieldNotInWireException of Proto.protoFieldDef
	fun vectorToList vec = 
		let fun inner (vec,n) = if (Word8Vector.length(vec) = n)
								then []
								else Word8Vector.sub(vec,n)::(inner(vec,n+1))
				in
					inner(vec,0)
				end
	fun realWithIntRepresentation x = Bits.toInt(vectorToList (PackRealLittle.toBytes x))
	fun realFromIntRepresentation32 x = PackRealLittle.fromBytes (Word8Vector.fromList(Bits.fromInt32 x))
	fun realFromIntRepresentation64 x = PackRealLittle.fromBytes (Word8Vector.fromList(Bits.fromInt64 x))
	fun bytesToString x = Byte.bytesToString(Word8Vector.fromList x)
	fun encodeProtoValue(v)	= 
		case v of
			Proto.Int32(x) => Wire.Varint(x)
		  | Proto.Int64(x) => Wire.Varint(x)
		  | Proto.UInt32(x) => Wire.Varint(x)
		  | Proto.UInt64(x) => Wire.Varint(x)
		  | Proto.Fixed32(x) => Wire.I32(x)
		  | Proto.Fixed64(x) => Wire.I64(x)
		  | Proto.SFixed32(x) => Wire.I32(x)
		  | Proto.SFixed64(x) => Wire.I64(x)
		  | Proto.SInt32(x) => Wire.Varint(ZigZag.encodeZigZag32(x))
		  | Proto.SInt64(x) => Wire.Varint(ZigZag.encodeZigZag64(x))
		  | Proto.Bytes(x) => Wire.Bytes(x)
		  | Proto.String(x) => Wire.Bytes(vectorToList(Byte.stringToBytes(x)))
		  | Proto.Bool(true) => Wire.Varint(1)
		  | Proto.Bool(false) => Wire.Varint(0)
		  | Proto.Float(x) => Wire.I32(realWithIntRepresentation(x))
		  | Proto.Double(x) => Wire.I64(realWithIntRepresentation(x))
		  | Proto.ProtoMessageValue(x) => Wire.WireMessage(encodeProtoMessage x)
	and encodeProtoField(Proto.Field(_,value,name,key)) = (key,encodeProtoValue(value))
	and encodeProtoMessage(Proto.Message(_,_,fields)) = map encodeProtoField fields
	
	fun decodeProtoValue(t,Wire.Varint(x)) =
			(case t of
				Proto.TInt32 => SOME(Proto.Int32(x))
			|	Proto.TInt64 => SOME(Proto.Int64(x))
			|	Proto.TUInt32 => SOME(Proto.UInt32(x))
			|	Proto.TUInt64 => SOME(Proto.UInt64(x))
			|	Proto.TBool => SOME(Proto.Bool(x = 0))
			|   _ => NONE)
	|	decodeProtoValue (t,Wire.I32(x)) = (
			case t of
				Proto.TFixed32 => SOME(Proto.Fixed32(x))
			|	Proto.TSFixed32 => SOME(Proto.SFixed32(x))
			|	Proto.TFloat	=> SOME(Proto.Float(realFromIntRepresentation32(x)))
			| 	_	=> NONE)
	|	decodeProtoValue(t,Wire.I64(x)) = (
			case t of
				Proto.TFixed64 => SOME(Proto.Fixed64(x))
			|	Proto.TSFixed64 => SOME(Proto.SFixed64(x))
			|	Proto.TDouble => SOME(Proto.Double(realFromIntRepresentation64(x)))
			|	_	=> NONE)
	|	decodeProtoValue(t,Wire.Bytes(x)) = (
			case t of
				Proto.TBytes => SOME(Proto.Bytes(x))
			|	Proto.TString => SOME(Proto.String(bytesToString(x)))
			|	Proto.TProtoMessage(msgDef) => 
					decodeProtoValue(Proto.TProtoMessage(msgDef)
					,Wire.WireMessage(WireEncoding.decodeWireMessageLs(x)))
			|	_	=> NONE)
	|	decodeProtoValue(t,Wire.WireMessage(x)) = (
			case t of
				Proto.TProtoMessage(protoDef) => 
					SOME(Proto.ProtoMessageValue(decodeProtoMessage(protoDef,x)))
			|	_	=> NONE)
			
	and decodeProtoField(fieldDef,wireVal) = 
		let val (Proto.FieldDef(opt,fieldType,name,key)) = fieldDef in
			case decodeProtoValue(fieldType,wireVal) of
				SOME(fieldValue) => Proto.Field(opt,fieldValue,name,key)
			|	NONE	=>	raise DecodingFieldException(fieldDef,wireVal,fieldType)
		end
		
	and findAndDecodeFieldInWire(field,[]) = raise FieldNotInWireException(field)
	  | findAndDecodeFieldInWire(field,w::ws) = 
		if(Proto.protoFieldDefKey(field) = Wire.wireKey(w))
		then (decodeProtoField(field,Wire.wireVal w),ws)
		else let val (x,rest) = findAndDecodeFieldInWire(field,ws) in
			(x,w::rest)
		end
		
	and decodeProtoFields([],_) = []
	  | decodeProtoFields(x,[]) = raise EndOfWireMessageException(x)
	  | decodeProtoFields(df::dfs,ws) = 
		let val (encoded,rest) = findAndDecodeFieldInWire(df,ws) in
			encoded::decodeProtoFields(dfs,ws)
		end
	and decodeProtoMessage(Proto.MessageDef(name,opts,fieldDefs),wireMsg) = 
		Proto.Message(name,opts,decodeProtoFields(fieldDefs,wireMsg))
		
	fun encodeMessageToStream msg = WireEncoding.encodeWireMessage(encodeProtoMessage(msg))
	fun decodeMessageFromStream (def,stream) = decodeProtoMessage(def,WireEncoding.decodeWireMessage(stream))
end :
sig
	val encodeProtoValue : Proto.protoValue -> Wire.wireValue
	val encodeProtoField : Proto.protoField -> Wire.wireField
	val encodeProtoMessage : Proto.protoMessage -> Wire.wireMessage
	val decodeProtoValue : Proto.protoType * Wire.wireValue -> Proto.protoValue option
	val decodeProtoField : Proto.protoFieldDef * Wire.wireValue -> Proto.protoField
	val decodeProtoMessage : Proto.protoMessageDef * Wire.wireMessage -> Proto.protoMessage
	val encodeMessageToStream : Proto.protoMessage -> Word8.word list
	val decodeMessageFromStream : Proto.protoMessageDef * Word8.word list -> Proto.protoMessage
end