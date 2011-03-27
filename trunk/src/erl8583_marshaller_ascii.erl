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
%% @doc This module marshals an iso8583message() into 
%%      an ASCII string or unmarshals an ASCII string into an
%%      iso8583message().

-module(erl8583_marshaller_ascii).

%%
%% Include files
%%
%% @headerfile "../include/erl8583_types.hrl"
-include("erl8583_types.hrl").

%%
%% Exported Functions
%%
-export([marshal/1, marshal/2, unmarshal/1, unmarshal/2, extract_fields/1]).

%%
%% API Functions
%%

%% @doc Marshals an ISO 8583 message into an ASCII string. This function
%%      uses the erl8583_marshaller_ascii_field module to marshal the fields.
%%
%% @spec marshal(iso8583message()) -> string()
-spec(marshal(iso8583message()) -> string()).

marshal(Message) ->
	marshal(Message, erl8583_marshaller_ascii_field).

%% @doc Marshals an ISO 8583 message into an ASCII string. This function
%%      uses the specified field marshalling module.
%%
%% @spec marshal(iso8583message(), module()) -> string()
-spec(marshal(iso8583message(), module()) -> string()).

marshal(Message, FieldMarshaller) ->
	Mti = erl8583_message:get(0, Message),
	[0|Fields] = erl8583_message:get_fields(Message),
	Mti ++ erl8583_marshaller_ascii_bitmap:marshal(Message) ++ encode(Fields, Message, FieldMarshaller).
	
%% @doc Unmarshals an ASCII string into an ISO 8583 message. This function
%%      uses the erl8583_marshaller_ascii_field module to unmarshal the fields.
%%
%% @spec unmarshal(string()) -> iso8583message()
-spec(unmarshal(string()) -> iso8583message()).

unmarshal(AsciiMessage) ->
	unmarshal(AsciiMessage, erl8583_marshaller_ascii_field).

%% @doc Unmarshals an ASCII string into an ISO 8583 message. This function
%%      uses the specified field marshalling module.
%%
%% @spec unmarshal(string(), module()) -> iso8583message()
-spec(unmarshal(string(), module()) -> iso8583message()).

unmarshal(AsciiMessage, FieldMarshaller) ->
	IsoMsg1 = erl8583_message:new(),
	{Mti, Rest} = lists:split(4, AsciiMessage),
	IsoMsg2 = erl8583_message:set(0, Mti, IsoMsg1),
	{FieldIds, Fields} = extract_fields(Rest),
	decode_fields(FieldIds, Fields, IsoMsg2, FieldMarshaller).

%% @doc Extracts a list of field IDs from an ASCII string 
%%      representation of an ISO 8583 message. The result is returned
%%      as a 2-tuple of the field IDs and the remainder of the 
%%      the message (encoding the field values but not the bit map).
%%
%% @spec extract_fields(string()) -> list(integer())
-spec(extract_fields(string()) -> list(integer())).

extract_fields([]) ->
	{[], []};
extract_fields(AsciiMessage) ->
	BitMapLength = get_bit_map_length(AsciiMessage),
	{AsciiBitMap, Fields} = lists:split(BitMapLength, AsciiMessage),
	BitMap = erl8583_convert:ascii_hex_to_string(AsciiBitMap),
	extract_fields(BitMap, 0, 8, {[], Fields}).

%%
%% Local Functions
%%
encode(Fields, Msg, FieldMarshaller) ->
	lists:reverse(encode(Fields, Msg, [], FieldMarshaller)).

encode([], _Msg, Result, _FieldMarshaller) ->
	Result;
encode([FieldId|Tail], Msg, Result, FieldMarshaller) ->
	Value = erl8583_message:get(FieldId, Msg),
	EncodedValue = FieldMarshaller:marshal(FieldId, Value),
	encode(Tail, Msg, lists:reverse(EncodedValue) ++ Result, FieldMarshaller).
 
extract_fields([], _Offset, _Index, {FieldIds, Fields}) ->
	Ids = lists:sort(FieldIds),
	{[Id || Id <- Ids, Id rem 64 =/= 1], Fields};
extract_fields([_Head|Tail], Offset, 0, {FieldIds, Fields}) ->
	extract_fields(Tail, Offset+1, 8, {FieldIds, Fields});
extract_fields([Head|Tail], Offset, Index, {FieldIds, Fields}) ->
	case Head band (1 bsl (Index-1)) of
		0 ->
			extract_fields([Head|Tail], Offset, Index-1, {FieldIds, Fields});
		_ ->
			extract_fields([Head|Tail], Offset, Index-1, {[Offset*8+9-Index|FieldIds], Fields})
	end.
			
decode_fields([], _, Result, _FieldMarshaller) ->
	Result;
decode_fields([FieldId|Tail], Fields, Result, FieldMarshaller) ->
	{Value, UpdatedFields} = FieldMarshaller:unmarshal(FieldId, Fields),
	UpdatedResult = erl8583_message:set(FieldId, Value, Result),
	decode_fields(Tail, UpdatedFields, UpdatedResult, FieldMarshaller).
	
get_bit_map_length(Msg) ->
	get_bit_map_length(Msg, 16).

get_bit_map_length(Msg, Length) ->
	[HexDig1, HexDig2|_Tail] = Msg,
	<<Byte>> = erl8583_convert:ascii_hex_to_binary([HexDig1, HexDig2]),
	case (Byte band 128) of
		0 ->
			Length;
		_ ->
			{_Msg1, Msg2} = lists:split(16, Msg),
			get_bit_map_length(Msg2, Length+16)
	end.
