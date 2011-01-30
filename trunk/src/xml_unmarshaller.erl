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
	unmarshall(ChildNodes, iso8583_message:new()).


%%
%% Local Functions
%%
unmarshall([], Iso8583Msg) ->
	Iso8583Msg;
unmarshall([Field|T], Iso8583Msg) when is_record(Field, xmlElement) ->
	field = Field#xmlElement.name,
	Attributes = Field#xmlElement.attributes,
	[Attr1, Attr2] = Attributes,
	case Attr1#xmlAttribute.name of
		id ->
			Id = Attr1#xmlAttribute.value,
			value = Attr2#xmlAttribute.name,
			Value = Attr2#xmlAttribute.value;
		value ->
			Id = Attr2#xmlAttribute.value,
			value = Attr1#xmlAttribute.name,
			Value = Attr1#xmlAttribute.value
	end,
	UpdatedMsg = iso8583_message:set(list_to_integer(Id), Value, Iso8583Msg),
	unmarshall(T, UpdatedMsg);
unmarshall([_H|T], Iso8583Msg) ->
	unmarshall(T, Iso8583Msg).
