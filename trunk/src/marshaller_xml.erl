% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

%% @author CA Meijer
%% @copyright 2011 CA Meijer
%% @doc marshaller_xml. This module marshalls an iso8583message 
%%      into an XML element. The XML elements created by this
%%      module should be compatible with jPOS.

-module(marshaller_xml).

%%
%% Include files
%%
%% @headerfile "../include/erl8583_types.hrl"
-include_lib("xmerl/include/xmerl.hrl").
-include("erl8583_types.hrl").

%%
%% Exported Functions
%%
-export([marshal/1, marshal/2, unmarshal/1, unmarshal/2]).

%%
%% API Functions
%%

%% @doc Marshals an ISO8583 message into an XML element with
%%      root tag &lt;iso8583message&gt;. The individual fields
%%      are marshalled using the marshaller_xml_field module.
%%
%% @spec marshal(iso8583message()) -> string()
-spec(marshal(iso8583message()) -> string()).

marshal(IsoMsg) ->
	marshal(IsoMsg, marshaller_xml_field).

%% @doc Marshals an ISO8583 message into an XML element with
%%      root tag &lt;iso8583message&gt;. The individual fields
%%      are marshalled using the specified marshaller.
%%
%% @spec marshal(iso8583message(), module()) -> string()
-spec(marshal(iso8583message(), module()) -> string()).

marshal(IsoMsg, FieldMarshaller) ->
	"<isomsg" ++ 
		encode_attributes(erl8583_message:get_attributes(IsoMsg)) ++ 
		">" ++ 
		marshal_fields(erl8583_message:to_list(IsoMsg), [], FieldMarshaller) ++ 
		"</isomsg>\n".
	
%% @doc Unmarshals an XML element with root tag &lt;iso8583message&gt;
%%      into an ISO 8583 message. The individual fields
%%      are unmarshalled using the marshaller_xml_field module.
%%
%% @spec unmarshal(string()) -> iso8583message()
-spec(unmarshal(string()) -> iso8583message()).

unmarshal(XmlMessage) ->
	unmarshal(XmlMessage, marshaller_xml_field).

%% @doc Unmarshals an XML element with root tag &lt;iso8583message&gt;
%%      into an ISO 8583 message. The individual fields
%%      are unmarshalled using the specified marshaller.
%%
%% @spec unmarshal(string(), module()) -> iso8583message()
-spec(unmarshal(string(), module()) -> iso8583message()).

unmarshal(XmlMessage, FieldMarshaller) ->
	{Xml, []} = xmerl_scan:string(XmlMessage),
	isomsg = Xml#xmlElement.name,
	ChildNodes = Xml#xmlElement.content,
	Msg = unmarshal(ChildNodes, erl8583_message:new(), FieldMarshaller),
	Attrs = Xml#xmlElement.attributes,
	erl8583_message:set_attributes(attributes_to_list(Attrs, []), Msg).

%%
%% Local Functions
%%
marshal_fields([], Result, _FieldMarshaller) ->
	Result;
marshal_fields([{FieldId, Value}|Tail], Result, FieldMarshaller) ->
	MarshalledValue = FieldMarshaller:marshal(FieldId, Value),
	marshal_fields(Tail, MarshalledValue ++ Result, FieldMarshaller).
	
encode_attributes(List) ->
	encode_attributes(List, "").

encode_attributes([], Result) ->
	Result;
encode_attributes([{Key, Value} | Tail], Result) ->
	encode_attributes(Tail, " " ++ Key ++ "=\"" ++ Value ++ "\"" ++  Result).

unmarshal([], Iso8583Msg, _FieldMarshaller) ->
	Iso8583Msg;
unmarshal([Field|T], Iso8583Msg, FieldMarshaller) when is_record(Field, xmlElement) ->
	Attributes = Field#xmlElement.attributes,
	AttributesList = attributes_to_list(Attributes, []),
	Id = get_attribute_value("id", AttributesList),
	FieldId = list_to_integer(Id),
	Value = FieldMarshaller:unmarshal(FieldId, Field),
	UpdatedMsg = erl8583_message:set(FieldId, Value, Iso8583Msg),
	unmarshal(T, UpdatedMsg, FieldMarshaller);
unmarshal([_H|T], Iso8583Msg, FieldMarshaller) ->
	unmarshal(T, Iso8583Msg, FieldMarshaller).

attributes_to_list([], Result) ->
	Result;
attributes_to_list([H|T], Result) ->
	Id = atom_to_list(H#xmlAttribute.name),
	Value = H#xmlAttribute.value,
	attributes_to_list(T, [{Id, Value} | Result]).

get_attribute_value(Key, [{Key, Value} | _Tail]) ->
	Value;
get_attribute_value(Key, [_Head|Tail]) ->
	get_attribute_value(Key, Tail).
