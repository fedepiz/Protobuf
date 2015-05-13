use "Load.sml";
exception TestFailedException;
open Lens;
open ProtoLens;
val source = String.concat
["message Country = { required string countryName = 1; } ",
 "message Maker = { required string makerName = 1; required Country makerCountry = 2; }",
 "message Car = { required string modelName = 1; required int32 year = 2; required Maker modelMaker = 3; }"]

val result = ProtoParser.parseMessages(source)
val (countryDef,makerDef,carDef) = case result of
				Parser.Success(x) => (hd x,hd(tl x),hd(tl(tl x)))
			|	Parser.Failure _ => raise TestFailedException
open ProtoAccess
val country = setStringValue("countryName","Italy",Proto.bareMessage countryDef)
val maker1 = setStringValue("makerName","Fiat",Proto.bareMessage makerDef)
val maker2 = setProtoMessageValue("makerCountry",country,maker1)
val car1 = setStringValue("modelName","Panda",Proto.bareMessage carDef)
val car2 = setIntValue("year",2008,car1)
val car3 = setProtoMessageValue("modelMaker",maker2,car2)
(*Let's now attempt to increment the car's year*)
val car4 = setIntValue("year",getIntValue("year",car3),car3)
(*And let's update the maker, who changed name!*)
val car5 = setProtoMessageValue
("modelMaker",
setStringValue("makerName","FCA",getProtoMessageValue("modelMaker",car4))
,car4)
(*Let's attempt to append a ! to the current country name*)
val currentCountry = getProtoMessageValue("makerCountry",getProtoMessageValue("modelMaker",car5))
val changedName = String.concat[getStringValue("countryName",currentCountry),"!"]
val changedCountry = setStringValue("countryName",changedName,currentCountry)
val changedMaker = setProtoMessageValue("makerCountry",changedCountry,
					getProtoMessageValue("modelMaker",car5))
val car6 = setProtoMessageValue("modelMaker",changedMaker,car5)

(*First, lenses needs to be built.*)
val countryName = stringLens "countryName"
val makerName = stringLens "makerName"
val makerCountry = protoMessageLens "makerCountry"
val modelName = stringLens "modelName"
val year = intLens "year"
val modelMaker = protoMessageLens "modelMaker"
(*now, we can procede to build things*)
val country = set countryName ("Italy",Proto.bareMessage countryDef)
val maker = sequence[setM makerName "Fiat",setM makerCountry country] (Proto.bareMessage makerDef)
val car = sequence[setM modelName "Panda",setM year 2008, setM modelMaker maker] (Proto.bareMessage carDef)
(*Increment car's year*)
val car2 = modify (fn x => x + 1) year car
(*And now update the maker*)
val car3 = set (modelMaker +> makerName) ("FCA",car2)
(*Apped ! to country name*)
val car4 = modify (fn x => String.concat[x,"!"]) (modelMaker +> makerCountry +> countryName) 