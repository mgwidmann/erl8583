%% Author: carl
%% Created: 29 Jan 2011
%% Description: TODO: Add description to xml_unmarshaller
-module(xml_unmarshaller).

%%
%% Include files
%%
-include_lib("xmerl/include/xmerl.hrl").


%%
%% Exported Functions
%%
-export([unmarshall/1]).

%%
%% API Functions
%%
unmarshall(XmlMessage) ->
	{Xml, []} = xmerl_scan:string(XmlMessage),
	isomsg = Xml#xmlElement.name,
	ChildNodes = Xml#xmlElement.content,
	Msg = unmarshall(ChildNodes, iso8583_message:new()),
	Attrs = Xml#xmlElement.attributes,
	iso8583_message:set_attributes(attributes_to_list(Attrs, []), Msg).


%%
%% Local Functions
%%
unmarshall([], Iso8583Msg) ->
	Iso8583Msg;
unmarshall([Field|T], Iso8583Msg) when is_record(Field, xmlElement) ->
	case Field#xmlElement.name of
		field ->
			Attributes = Field#xmlElement.attributes,
			AttributesList = attributes_to_list(Attributes, []),
			Id = get_attribute_value("id", AttributesList),
			ValueStr = get_attribute_value("value", AttributesList),
			case is_attribute("type", AttributesList) of
				false ->
					Value = ValueStr;
				true ->
					"binary" = get_attribute_value("type", AttributesList),
					Value = convert:ascii_hex_to_binary(ValueStr)
			end;
		isomsg ->
			Attrs = Field#xmlElement.attributes,
			AttrList = attributes_to_list(Attrs, []),
			Id = get_attribute_value("id", AttrList),
			AttrsExceptId = AttrList -- [{"id", Id}],
			ChildNodes = Field#xmlElement.content,
			Value = unmarshall(ChildNodes, iso8583_message:new(AttrsExceptId))
	end,	
	UpdatedMsg = iso8583_message:set(list_to_integer(Id), Value, Iso8583Msg),
	unmarshall(T, UpdatedMsg);
unmarshall([_H|T], Iso8583Msg) ->
	unmarshall(T, Iso8583Msg).

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
