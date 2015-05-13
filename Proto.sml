structure Proto =
struct
	exception WrongFieldType
	exception NoSuchField
	datatype protoRule = Required | Optional | Repeated | PackedRepeated
	and protoType = TDouble | TFloat | TInt32 | TInt64
					   | TUInt32 | TUInt64 | TSInt32 | TSInt64
					   | TFixed32 | TFixed64 | TSFixed32 | TSFixed64
					   | TBool | TString | TBytes 
					   | TProtoMessage of protoMessageDef
					   
	and protoValue = Double of real | Float of real | Int32 of int | Int64 of int
					   | UInt32 of int | UInt64 of int | SInt32 of int | SInt64 of int
					   | Fixed32 of int| Fixed64 of int | SFixed32 of int | SFixed64 of int
					   | Bool of bool | String of string| Bytes of Word8.word list | ProtoMessageValue of protoMessage
	and protoField = Field of protoRule * protoValue * string * int
	and protoFieldDef = FieldDef of protoRule * protoType * string * int
	and optionalEntry = Nothing (* For now at least*)
	and protoMessage = Message of string * optionalEntry list * protoField list
	and protoMessageDef = MessageDef of string * optionalEntry list * protoFieldDef list

	fun protoFieldDefKey(FieldDef(_,_,_,k)) = k
	fun protoFieldKey(Field(_,_,_,k)) = k
	fun fieldValue (Field(_,x,_,_)) = x
	fun fieldType (Double _) = TDouble
	  | fieldType (Float _) = TFloat
	  | fieldType (Int32 _) = TInt32
	  | fieldType (Int64 _) = TInt64
	  | fieldType (UInt32 _) = TUInt32
	  | fieldType (UInt64 _) = TUInt64
	  | fieldType (Fixed32 _) = TFixed32
	  | fieldType (Fixed64 _) = TFixed64
	  | fieldType (SFixed32 _) = TSFixed32
	  | fieldType (SFixed64 _) = TSFixed64
	  | fieldType (SInt32 _) = TSInt32
	  | fieldType (SInt64 _) = TSInt64
	  | fieldType (Bool _) = TBool
	  | fieldType (String _) = TString
	  | fieldType (Bytes _) = TBytes
	  | fieldType (ProtoMessageValue(Message(name,opt,fields))) =
		TProtoMessage(MessageDef(name,opt,map fieldDef fields))
	and fieldDef (Field(rule,value,name,k)) = FieldDef(rule, fieldType value,name,k)
	fun typeIsVarintEncoded(TInt32) = true
	  | typeIsVarintEncoded(TInt64) = true
	  | typeIsVarintEncoded(TUInt32) = true
	  | typeIsVarintEncoded(TUInt64) = true
	  | typeIsVarintEncoded(TBool) = true
	  | typeIsVarintEncoded _ = false
	  
	fun typeIsZigZagEncoded(TSInt32) = true
	  | typeIsZigZagEncoded(TSInt64) = true
	  | typeIsZigZagEncoded _ = false
	  
	fun typeIsFixedEncoded32(TFixed32) = true
	  | typeIsFixedEncoded32(TFloat) = true
	  | typeIsFixedEncoded32(TSFixed32) = true
	  | typeIsFixedEncoded32 _ = false
	  
	fun typeIsFixedEncoded64(TFixed32) = true
	  | typeIsFixedEncoded64(TFloat) = true
	  | typeIsFixedEncoded64(TSFixed32) = true
	  | typeIsFixedEncoded64 _ = false
	  
	fun typeIsLenEncoded(TBytes) = true
	  | typeIsLenEncoded(TString) = true
	  | typeIsLenEncoded _ = false
	fun messageName(Message(n,_,_)) = n
	fun messageDefName(MessageDef(n,_,_)) = n
	fun fieldDef(Field(rule,value,name,key)) = FieldDef(rule,fieldType value,name,key)
	fun fieldFromDef(FieldDef(rule,_,name,key),x) = Field(rule,x,name,key)
	
	fun messageDef(Message(name,opt,fields)) = MessageDef(name,opt,map fieldDef fields)
	
	
	(*Debug Stuff*)
	fun emptyMessage n = Message(n,[],[])
	
	fun bareValue (TDouble) = Double 0.0
	  | bareValue (TFloat) = Float 0.0
	  | bareValue (TInt32) = Int32 0
	  | bareValue (TInt64) = Int64 0
	  | bareValue (TUInt32) = UInt32 0
	  | bareValue (TUInt64) = UInt64 0
	  | bareValue (TFixed32) = Fixed32 0
	  | bareValue (TFixed64) = Fixed64 0
	  | bareValue (TSFixed32) = SFixed32 0
	  | bareValue (TSFixed64) = SFixed64 0
	  | bareValue (TSInt32) = SInt32 0
	  | bareValue (TSInt64) = SInt64 0
	  | bareValue (TBool) = Bool false
	  | bareValue (TString) = String ""
	  | bareValue (TBytes) = Bytes []
	  | bareValue (TProtoMessage(MessageDef(name,_,_))) = ProtoMessageValue(emptyMessage name)
	fun bareField(FieldDef(rule,value,name,key)) = Field(rule,bareValue(value),name,key)
    fun bareMessage(MessageDef(name,opt,fields)) = Message(name,opt,map bareField fields)
			
	fun simpleMessage(x) = Message("TestMessage",[],[x])
	fun simpleIntMessage(v,n,k) = simpleMessage(Field(Required,Int32(v),n,k))
	val sampleMessage = simpleIntMessage(5,"test",1)
	val sampleMessage2 = Message("Sample2",[],[(Field(Required,Int32(0),"first",1)),
											   (Field(Required,String("bla"),"second",2))])
	fun fold _ x [] = x 
      | fold _ _ (x::[]) = x
	  | fold f default (x::y::ls) = fold f default (f(x,y)::ls)
	fun lineBreakConcat (x,y) = String.concat[x,"\n",y]
	fun valueToString value =
		(case value of
			Double x => (Real.toString x)
		|	Float x => (Real.toString x)
		|	Int32 x => (Int.toString x)
		|	Int64 x => (Int.toString x)
		|	UInt32 x => (Int.toString x)
		|	UInt64 x => (Int.toString x)
		|	SInt32 x => (Int.toString x)
		|	SInt64 x => (Int.toString x)
		|	Fixed32 x => (Int.toString x)
		|	SFixed32 x => (Int.toString x)
		|	Bool x => (Bool.toString x)
		|	String x => (x)
		|	Bytes x => (String.concat(map Word8.toString x))
		|	ProtoMessageValue x => messageToString x)
	and messageToString (Message(name,_,fields)) =  
		let val strFields = map (fn x => valueToString(fieldValue x)) fields
			val body = fold lineBreakConcat "" strFields in
				String.concat[name,"{\n",body,"\n}"]
			end
			
	
end
(*This structure contains type checked access*)
structure ProtoAccess = 
struct
	open Proto
	fun getNativeIntValue(p) = 
		case p of
			Int32 x => x
		|	Int64 x => x
		|   UInt32 x => x
		|	UInt64 x => x
		|	SInt32 x => x
		|	SInt64 x => x
		|	Fixed32 x => x
		|	Fixed64 x => x
		|	SFixed32 x => x
		|	SFixed64 x => x
		|	_ => raise WrongFieldType
	
	fun getNativeRealValue(Double x) = x
	  | getNativeRealValue(Float x) = x
	  | getNativeRealValue _ = raise WrongFieldType
	
	fun getNativeStringValue(String x) = x
	  | getNativeStringValue _ = raise WrongFieldType
	
	fun getNativeBytesValue (Bytes x) = x
	  | getNativeBytesValue _ = raise WrongFieldType
	  
	fun getNativeBoolValue (Bool x) = x
	  | getNativeBoolValue _ = raise WrongFieldType
	  
	fun getNativeProtoMessageValue(ProtoMessageValue x) = x
	  | getNativeProtoMessageValue _ = raise WrongFieldType
	
	fun setNativeIntValue x p =
		case p of
			Int32 _ => Int32 x
		|	Int64 _ => Int64 x
		|	UInt32 _ => UInt32 x
		|	UInt64 _ => UInt64 x
		|	SInt32 _ => SInt32 x
		|	SInt64 _ => SInt64 x
		|	Fixed32 _ => Fixed32 x
		|	Fixed64 _ => Fixed64 x
		|	SFixed32 _ => SFixed32 x
		|	SFixed64 _ => SFixed64 x
		|	_ => raise WrongFieldType
	
	fun setNativeRealValue x p =
		case p of 
			Float _ => Float x
		|	Double _ => Double x
		|	_	=> raise WrongFieldType
	
	fun setNativeStringValue x (String _) = String x
	  | setNativeStringValue _ _ = raise WrongFieldType
	  
	fun setNativeBytesValue x (Bytes _) = Bytes x
	  | setNativeBytesValue _ _ = raise WrongFieldType
	
	fun setNativeBoolValue x (Bool _) = Bool x
      | setNativeBoolValue _ _ = raise WrongFieldType

	fun setNativeProtoMessageValue x (ProtoMessageValue _) = ProtoMessageValue x
	  | setNativeProtoMessageValue _ _ = raise WrongFieldType
	  
	fun getFieldValueInList (_,_,[]) = raise NoSuchField
	  | getFieldValueInList (f,name,field::fields) = 
		let val Field(_,value,n,_) = field in
			case n = name of
				false => getFieldValueInList(f,name,fields)
			|	true => f(value)
		end
	fun setFieldValueInList (_,_,[]) = raise NoSuchField
	  | setFieldValueInList (f,name,(Field(r,v,n,key))::fields) = 
			case name = n of
				false => Field(r,v,n,key)::(setFieldValueInList(f,name,fields))
			|	true => Field(r,f(v),n,key)::fields
	fun getFieldValue f (n,Message(_,_,l)) = getFieldValueInList(f,n,l)
	fun setFieldValue f (n,Message(x,y,l)) = Message(x,y,setFieldValueInList(f,n,l))
	
	fun getIntValue x = getFieldValue (getNativeIntValue) x
	fun setIntValue (n,v,m) = setFieldValue (setNativeIntValue v) (n,m)
	
	fun getRealValue x = getFieldValue (getNativeRealValue) x
	fun setRealValue (n,v,m) = setFieldValue (setNativeRealValue v) (n,m)
	
	fun getStringValue x = getFieldValue (getNativeStringValue) x
	fun setStringValue (n,v,m) = setFieldValue (setNativeStringValue v) (n,m)
	
	fun getBoolValue x = getFieldValue (getNativeBoolValue) x
	fun setBoolValue (n,v,m) = setFieldValue (setNativeBoolValue v) (n,m)
	
	fun getBytesValue x = getFieldValue (getNativeBytesValue) x
	fun setBytesValue (n,v,m) = setFieldValue (setNativeBytesValue v) (n,m)
	
	fun getProtoMessageValue x = getFieldValue (getNativeProtoMessageValue) x
	fun setProtoMessageValue (n,v,m) = setFieldValue (setNativeProtoMessageValue v) (n,m)
end :
sig
	val getFieldValue : (Proto.protoValue -> 'a) -> string * Proto.protoMessage -> 'a
	val setFieldValue : (Proto.protoValue -> Proto.protoValue) -> string * Proto.protoMessage -> Proto.protoMessage
	val getIntValue : string * Proto.protoMessage -> int
	val setIntValue : string * int * Proto.protoMessage -> Proto.protoMessage
	val getRealValue : string * Proto.protoMessage -> real
	val setRealValue : string * real * Proto.protoMessage -> Proto.protoMessage
	val getStringValue : string * Proto.protoMessage -> string 
	val setStringValue : string * string * Proto.protoMessage -> Proto.protoMessage
	val getBoolValue : string * Proto.protoMessage -> bool
	val setBoolValue : string * bool * Proto.protoMessage -> Proto.protoMessage
	val getBytesValue : string * Proto.protoMessage -> Word8.word list
	val setBytesValue : string * Word8.word list * Proto.protoMessage -> Proto.protoMessage
	val getProtoMessageValue : string * Proto.protoMessage -> Proto.protoMessage
	val setProtoMessageValue : string * Proto.protoMessage * Proto.protoMessage -> Proto.protoMessage
end