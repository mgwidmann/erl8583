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
%% @doc This module marshals an iso8583message() 
%%      into a JSON document and can unmarshal a JSIBL document into
%%      an iso8583message(). This module also exposes
%%      functions for explicitly marshalling/unmarshalling
%%      the MTI, bitmap and fields of a message. This is
%%      to conform to the design of other marshallers and
%%      the contract required by the erl8583_marshaller.
-module(erl8583_marshaller_json).

%%
%% Include files
%%
%% @headerfile "../include/erl8583_types.hrl"
-include("erl8583_types.hrl").
-include("erl8583_marshallers.hrl").

%%
%% Exported Functions
%%
-export([marshal/1,
		 unmarshal/1,
		 unmarshal_mti/1,
		 unmarshal_bitmap/1,
		 unmarshal_field/3]).

%%
%% API Functions
%%

%% @doc Constructs a JSON document of
%%      an iso8583message().
%%
%% @spec marshal(iso8583message()) -> string()
-spec(marshal(iso8583message()) -> string()).

marshal(Message) ->
	erl8583_marshaller:marshal(Message, ?MARSHALLER_JSON).

%% @doc Constructs an iso8583message() from a JSON document 
%%      representation of the message.
%%
%% @spec unmarshal(string()) -> iso8583message()
-spec(unmarshal(string()) -> iso8583message()).

unmarshal(Marshalled) ->
	erl8583_marshaller:unmarshal(Marshalled, ?MARSHALLER_JSON).

%% @doc Extracts the MTI from a JSON document.
%%      The MTI and the XML document are 
%%      returned as a 2-tuple.
%%
%% @spec unmarshal_mti(string()) -> {string(), string()}
-spec(unmarshal_mti(string()) -> {string(), string()}).

unmarshal_mti(Marshalled) ->
	{struct, JsonData} = mochijson2:decode(Marshalled),
	{struct, FieldsData} = proplists:get_value(<<"fields">>, JsonData),
	MtiBin = proplists:get_value(<<"0">>, FieldsData),
	{binary_to_list(MtiBin), Marshalled}.

%% @doc Extracts a list of field IDs from a JSON 
%%      representation of an ISO 8583 message. The result is returned
%%      as a 2-tuple of the field IDs and the JSON document.
%%
%% @spec unmarshal_bitmap(string()) -> {list(integer()), string()}
-spec(unmarshal_bitmap(string()) -> {list(integer()), string()}).

unmarshal_bitmap(Marshalled) ->
	{struct, JsonData} = mochijson2:decode(Marshalled),
	{struct, FieldsData} = proplists:get_value(<<"fields">>, JsonData),
	Fields = proplists:get_keys(FieldsData),
	FieldIds = [list_to_integer(binary_to_list(Id)) || Id <- Fields] -- [0],
	{lists:sort(FieldIds), Marshalled}.

%% @doc Extracts a field value for a specified field from a JSON
%%      document.  The field value and the JSON document are 
%%      returned as a 2-tuple.
%%
%% @spec unmarshal_field(integer(), string(), module()) -> {iso8583field_value(), string()}
-spec(unmarshal_field(integer(), string(), module()) -> {iso8583field_value(), string()}).

unmarshal_field(FieldId, Marshalled, EncodingRules) ->
	{struct, JsonData} = mochijson2:decode(Marshalled),
	{struct, FieldsProps} = proplists:get_value(<<"fields">>, JsonData),
	FieldValue = proplists:get_value(list_to_binary(integer_to_list(FieldId)), FieldsProps),
	case FieldValue of
		{struct, PropList} ->
			Value = unmarshal_complex_field([FieldId], erl8583_message:new(), PropList, EncodingRules);
		_ ->
			Value = unmarshal_simple_field(FieldId, FieldValue, EncodingRules)
	end,
	{Value, Marshalled}.


%%
%% Local Functions
%%
unmarshal_simple_field(FieldId, FieldValue, EncodingRules) ->
	case EncodingRules:get_encoding(FieldId) of
		{b, _, _} ->
			erl8583_convert:ascii_hex_to_binary(binary_to_list(FieldValue));
		_ ->
			binary_to_list(FieldValue)
	end.

unmarshal_complex_field(FieldId, Message, PropList, EncodingRules) ->
	ConstructMessageFun = fun(Id, MessageAccum) ->
								  IdInt = list_to_integer(binary_to_list(Id)),
								  UpdatedId = FieldId ++ [IdInt],
					  			  FieldValue = proplists:get_value(Id, PropList),
								  case FieldValue of
									  {struct, PropList2} ->
										  Value = unmarshal_complex_field(UpdatedId, 
																		  erl8583_message:new(),
																		  PropList2, 
																		  EncodingRules);
									  _ ->
										  Value = unmarshal_simple_field(UpdatedId, 
																		 FieldValue, 
																		 EncodingRules)
								  end,
					  			  erl8583_message:set(IdInt, Value, MessageAccum)
			  end,
	SubFieldIds = proplists:get_keys(PropList),
	lists:foldl(ConstructMessageFun, Message, SubFieldIds).
