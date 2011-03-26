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
%%      an ASCII string.

-module(erl8583_marshaller_ascii).

%%
%% Include files
%%
%% @headerfile "../include/erl8583_types.hrl"
-include("erl8583_types.hrl").

%%
%% Exported Functions
%%
-export([marshal/1, marshal/2, unmarshal/1, unmarshal/2, construct_bitmap/1, extract_fields/1]).

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
	Mti ++ construct_bitmap(Fields) ++ encode(Fields, Message, FieldMarshaller).
	
%% @doc Unmarshals an ASCII string into an ISO 8583 message. This function
%%      uses the erl8583_marshaller_ascii_field module to unmarshal the fields.
%%
%% @spec unmarshal(string()) -> iso8583message()
-spec(unmarshal(string()) -> iso8583message()).

unmarshal(Message) ->
	unmarshal(Message, erl8583_marshaller_ascii_field).

%% @doc Unmarshals an ASCII string into an ISO 8583 message. This function
%%      uses the specified field marshalling module.
%%
%% @spec unmarshal(string(), module()) -> iso8583message()
-spec(unmarshal(string(), module()) -> iso8583message()).

unmarshal(Message, FieldMarshaller) ->
	IsoMsg1 = erl8583_message:new(),
	{Mti, Rest} = lists:split(4, Message),
	IsoMsg2 = erl8583_message:set(0, Mti, IsoMsg1),
	{FieldIds, Fields} = extract_fields(Rest),
	decode_fields(FieldIds, Fields, IsoMsg2, FieldMarshaller).

%% @doc Constructs an ASCII hex string representation of the
%%      bitmap for a list of field IDs.
%%
%% @spec construct_bitmap(list(integer())) -> string()
-spec(construct_bitmap(list(integer())) -> string()).

construct_bitmap([]) ->
	[];
construct_bitmap(FieldIds) ->
	NumBitMaps = (lists:max(FieldIds) + 63) div 64,
	ExtensionBits = [Bit * 64 - 127 || Bit <- lists:seq(2, NumBitMaps)],
	BitMap = lists:duplicate(NumBitMaps * 8, 0),
	erl8583_convert:string_to_ascii_hex(construct_bitmap(lists:sort(ExtensionBits ++ FieldIds), BitMap)).

%% @doc Extracts a list of field IDs from an ASCII hex string 
%%      representation of the bitmap.
%%
%% @spec extract_fields(string()) -> list(integer())
-spec(extract_fields(string()) -> list(integer())).

extract_fields([]) ->
	{[], []};
extract_fields(Message) ->
	BitMapLength = get_bit_map_length(Message),
	{AsciiBitMap, Fields} = lists:split(BitMapLength, Message),
	BitMap = erl8583_convert:ascii_hex_to_string(AsciiBitMap),
	extract_fields(BitMap, 0, 8, {[], Fields}).

%%
%% Local Functions
%%
construct_bitmap([], Result) ->
	Result;
construct_bitmap([Field|Tail], Result) when Field > 0 ->
	ByteNum = (Field - 1) div 8,
	BitNum = 7 - ((Field - 1) rem 8),
	{Left, Right} = lists:split(ByteNum, Result),
	[ToUpdate | RightRest] = Right,
	construct_bitmap(Tail, Left ++ ([ToUpdate + (1 bsl BitNum)]) ++ RightRest).

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
