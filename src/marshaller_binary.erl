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
%% @doc marshaller_binary. This module marshalls an iso8583message into 
%%      a binary.

-module(marshaller_binary).

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

%% @doc Marshals an ISO 8583 message into a binary. This function uses
%%      the marshaller_binary_field module to marshal the fields.
%%
%% @spec marshal(iso8583message()) -> binary()
-spec(marshal(iso8583message()) -> binary()).

marshal(Msg) ->
	marshal(Msg, marshaller_binary_field).

%% @doc Marshals an ISO 8583 message into an ASCII string. This function
%%      uses the specified field marshalling module.
%%
%% @spec marshal(iso8583message(), module()) -> binary()
-spec(marshal(iso8583message(), module()) -> binary()).

marshal(Msg, FieldMarshaller) ->
	Mti = iso8583_message:get(0, Msg),
	MtiBits = convert:ascii_hex_to_binary(Mti),
	[0|Fields] = iso8583_message:get_fields(Msg),
	BitMap = construct_bitmap(Fields),
	EncodedFields = encode(Fields, Msg, FieldMarshaller),
	<< MtiBits/binary, BitMap/binary, EncodedFields/binary>>.

%% @doc Unmarshals a binary into an ISO 8583 message. This function uses
%%      the marshaller_binary_field module to unmarshal the fields.
%%
%% @spec unmarshal(binary()) -> iso8583message()
-spec(unmarshal(binary()) -> iso8583message()).

unmarshal(Msg) ->
	unmarshal(Msg, marshaller_binary_field).

%% @doc Unmarshals a binary into an ISO 8583 message. This function uses
%%      the specified field marshalling module.
%%
%% @spec unmarshal(binary(), module()) -> iso8583message()
-spec(unmarshal(binary(), module()) -> iso8583message()).

unmarshal(Msg, FieldMarshaller) ->
	IsoMsg1 = iso8583_message:new(),
	{MtiBin, Rest} = split_binary(Msg, 2),
	Mti = convert:binary_to_ascii_hex(MtiBin),
	IsoMsg2 = iso8583_message:set(0, Mti, IsoMsg1),
	{FieldIds, Fields} = extract_fields(Rest),
	decode_fields(FieldIds, Fields, IsoMsg2, FieldMarshaller).

%% @doc Constructs a binary representation of the bitmap for a list of 
%%      field IDs.
%%
%% @spec construct_bitmap(list(integer())) -> binary()
-spec(construct_bitmap(list(integer())) -> binary()).

construct_bitmap([]) ->
	<<>>;
construct_bitmap(Fields) ->
	NumBitMaps = (lists:max(Fields) + 63) div 64,
	ExtensionBits = [Bit * 64 - 127 || Bit <- lists:seq(2, NumBitMaps)],
	BitMap = lists:duplicate(NumBitMaps * 8, 0),
	construct_bitmap(lists:sort(ExtensionBits ++ Fields), BitMap).

%% @doc Extracts a list of field IDs from a binary representation of 
%%      the bitmap.
%%
%% @spec extract_fields(binary()) -> list(integer())
-spec(extract_fields(binary()) -> list(integer())).

extract_fields(<<>>) ->
	{[], <<>>};
extract_fields(Message) ->
	BitMapLength = get_bit_map_length(Message),
	{BinaryBitMap, Fields} = split_binary(Message, BitMapLength),
	BitMap = binary_to_list(BinaryBitMap),
	extract_fields(BitMap, 0, 8, {[], Fields}).

%%
%% Local Functions
%%
construct_bitmap([], Result) ->
	list_to_binary(Result);
construct_bitmap([Field|Tail], Result) when Field > 0 ->
	ByteNum = (Field - 1) div 8,
	BitNum = 7 - ((Field - 1) rem 8),
	{Left, Right} = lists:split(ByteNum, Result),
	[ToUpdate | RightRest] = Right,
	construct_bitmap(Tail, Left ++ ([ToUpdate + (1 bsl BitNum)]) ++ RightRest).

encode(Fields, Msg, FieldMarshaller) ->
	encode(Fields, Msg, <<>>, FieldMarshaller).

encode([], _Msg, Result, _FieldMarshaller) ->
	Result;
encode([FieldId|Tail], Msg, Result, FieldMarshaller) ->
	Value = iso8583_message:get(FieldId, Msg),
	EncodedValue = FieldMarshaller:marshal(FieldId, Value),
	encode(Tail, Msg, convert:concat_binaries(Result, EncodedValue), FieldMarshaller).

get_bit_map_length(Message) ->
	[Head|_Tail] = binary_to_list(Message),
	case Head >= 128 of
		false ->
			8;
		true ->
			{_, Rest} = erlang:split_binary(Message, 8),
			8 + get_bit_map_length(Rest)
	end.

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

decode_fields([], _, Result, _EncodingRules) ->
	Result;
decode_fields([FieldId|Tail], Fields, Result, FieldMarshaller) ->
	{Value, UpdatedFields} = FieldMarshaller:unmarshal(FieldId, Fields),
	UpdatedResult = iso8583_message:set(FieldId, Value, Result),
	decode_fields(Tail, UpdatedFields, UpdatedResult, FieldMarshaller).
