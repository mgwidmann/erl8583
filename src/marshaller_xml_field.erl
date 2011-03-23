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
%% @doc marshaller_xml_field. This module marshalls an iso8583message 
%%      field into an XML element.

-module(marshaller_xml_field).

%%
%% Include files
%%
%% @headerfile "../include/erl8583_types.hrl"
-include_lib("xmerl/include/xmerl.hrl").
-include("erl8583_types.hrl").

%%
%% Exported Functions
%%
-export([marshal/2, unmarshal/2]).

%%
%% API Functions
%%

%% @doc Marshals a field into an XML element.
%%
%% @spec marshal(integer(), iso8583field_value()) -> string()
-spec(marshal(integer(), iso8583field_value()) -> string()).

marshal(FieldId, Value) when is_list(Value)->
	Id = integer_to_list(FieldId),
	"<field id=\"" ++ Id ++ "\" value=\"" ++ Value ++ "\" />";
marshal(FieldId, Value) when is_binary(Value) ->
	Id = integer_to_list(FieldId),
	"<field id=\"" ++ 
		Id ++ 
		"\" value=\"" ++ 
		erl8583_convert:binary_to_ascii_hex(Value) ++
		"\" type=\"binary\" />";
% if we drop through to here, Value is of type iso8583message().
marshal(FieldId, Value) ->
	Id = integer_to_list(FieldId),
	"<isomsg id=\"" ++ 
		Id ++ 
		"\"" ++
		encode_attributes(erl8583_message:get_attributes(Value)) ++
		">" ++
		marshal_fields(erl8583_message:to_list(Value), "") ++ 
		"</isomsg>".

%% @doc Unarshals an XML element into a field value.
%%
%% @spec unmarshal(integer(), string()) -> iso8583field_value()
-spec(unmarshal(integer(), string()) -> iso8583field_value()).

unmarshal(_FieldId, FieldElement) ->
	Attributes = FieldElement#xmlElement.attributes,
	AttributesList = attributes_to_list(Attributes, []),
	Id = get_attribute_value("id", AttributesList),
	case FieldElement#xmlElement.name of
		field ->
			ValueStr = get_attribute_value("value", AttributesList),
			case is_attribute("type", AttributesList) of
				false ->
					ValueStr;
				true ->
					"binary" = get_attribute_value("type", AttributesList),
					erl8583_convert:ascii_hex_to_binary(ValueStr)
			end;
		isomsg ->
			AttrsExceptId = AttributesList -- [{"id", Id}],
			ChildNodes = FieldElement#xmlElement.content,
			unmarshal_complex(ChildNodes, erl8583_message:new(AttrsExceptId))
	end.	


%%
%% Local Functions
%%
encode_attributes(List) ->
	encode_attributes(List, "").

encode_attributes([], Result) ->
	Result;
encode_attributes([{Key, Value} | Tail], Result) ->
	encode_attributes(Tail, " " ++ Key ++ "=\"" ++ Value ++ "\"" ++  Result).

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

marshal_fields([], Result) ->
	Result;
marshal_fields([{FieldId, Value}|Tail], Result) ->
	MarshalledValue = marshal(FieldId, Value),
	marshal_fields(Tail, MarshalledValue ++ Result).

unmarshal_complex([], Iso8583Msg) ->
	Iso8583Msg;
unmarshal_complex([Field|T], Iso8583Msg) when is_record(Field, xmlElement) ->
	Attributes = Field#xmlElement.attributes,
	AttributesList = attributes_to_list(Attributes, []),
	Id = get_attribute_value("id", AttributesList),
	FieldId = list_to_integer(Id),
	Value = unmarshal(FieldId, Field),
	UpdatedMsg = erl8583_message:set(FieldId, Value, Iso8583Msg),
	unmarshal_complex(T, UpdatedMsg);
unmarshal_complex([_H|T], Iso8583Msg) ->
	unmarshal_complex(T, Iso8583Msg).
