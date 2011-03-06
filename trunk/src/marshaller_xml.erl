%% Author: carl
%% Created: 06 Feb 2011
%% Description: TODO: Add description to marshaller_xml
-module(marshaller_xml).

%%
%% Include files
%%
-include_lib("xmerl/include/xmerl.hrl").

%%
%% Exported Functions
%%
-export([marshal/1, unmarshal/1, marshal_fields/3]).

%%
%% API Functions
%%
marshal(IsoMsg) ->
	marshal(IsoMsg, marshaller_xml_field).

marshal(IsoMsg, FieldMarshaller) ->
	"<isomsg" ++ 
		encode_attributes(iso8583_message:get_attributes(IsoMsg)) ++ 
		">" ++ 
		marshal_fields(iso8583_message:to_list(IsoMsg), [], FieldMarshaller) ++ 
		"</isomsg>\n".
	
unmarshal(XmlMessage) ->
	{Xml, []} = xmerl_scan:string(XmlMessage),
	isomsg = Xml#xmlElement.name,
	ChildNodes = Xml#xmlElement.content,
	Msg = unmarshal(ChildNodes, iso8583_message:new()),
	Attrs = Xml#xmlElement.attributes,
	iso8583_message:set_attributes(attributes_to_list(Attrs, []), Msg).

marshal_fields([], Result, _FieldMarshaller) ->
	Result;
marshal_fields([{FieldId, Value}|Tail], Result, FieldMarshaller) ->
	MarshalledValue = FieldMarshaller:marshal(FieldId, Value),
	marshal_fields(Tail, MarshalledValue ++ Result, FieldMarshaller).

%%
%% Local Functions
%%
	
encode_attributes(List) ->
	encode_attributes(List, "").

encode_attributes([], Result) ->
	Result;
encode_attributes([{Key, Value} | Tail], Result) ->
	encode_attributes(Tail, " " ++ Key ++ "=\"" ++ Value ++ "\"" ++  Result).

unmarshal([], Iso8583Msg) ->
	Iso8583Msg;
unmarshal([Field|T], Iso8583Msg) when is_record(Field, xmlElement) ->
	Attributes = Field#xmlElement.attributes,
	AttributesList = attributes_to_list(Attributes, []),
	Id = get_attribute_value("id", AttributesList),
	FieldId = list_to_integer(Id),
	case Field#xmlElement.name of
		field ->
			ValueStr = get_attribute_value("value", AttributesList),
			case is_attribute("type", AttributesList) of
				false ->
					Value = ValueStr;
				true ->
					"binary" = get_attribute_value("type", AttributesList),
					Value = convert:ascii_hex_to_binary(ValueStr)
			end;
		isomsg ->
			AttrsExceptId = AttributesList -- [{"id", Id}],
			ChildNodes = Field#xmlElement.content,
			Value = unmarshal(ChildNodes, iso8583_message:new(AttrsExceptId))
	end,	
	UpdatedMsg = iso8583_message:set(FieldId, Value, Iso8583Msg),
	unmarshal(T, UpdatedMsg);
unmarshal([_H|T], Iso8583Msg) ->
	unmarshal(T, Iso8583Msg).

attributes_to_list([], Result) ->
	Result;
attributes_to_list([H|T], Result) ->
	Id = atom_to_list(H#xmlAttribute.name),
	Value = H#xmlAttribute.value,
	attributes_to_list(T, [{Id, Value} | Result]).

is_attribute(_Id, []) ->
	false;
is_attribute(Id, [{Id, _}|_Tail]) ->
	true;
is_attribute(Id, [_Head|Tail]) ->
	is_attribute(Id, Tail).

get_attribute_value(Key, [{Key, Value} | _Tail]) ->
	Value;
get_attribute_value(Key, [_Head|Tail]) ->
	get_attribute_value(Key, Tail).
