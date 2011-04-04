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
%% @doc This module marshalls an iso8583message() into 
%%      a binary.

-module(erl8583_marshaller_binary).

%%
%% Include files
%%
%% @headerfile "../include/erl8583_types.hrl"
-include("erl8583_types.hrl").
-include("erl8583_field_ids.hrl").

%%
%% Exported Functions
%%
-export([marshal/1, marshal/2, marshal/3, unmarshal/1, unmarshal/2, unmarshal/3]).
-export([marshal_bitmap/1, unmarshal_bitmap/1]).

%%
%% API Functions
%%

%% @doc Marshals an ISO 8583 message into a binary. This function uses
%%      the erl8583_marshaller_binary_field module to marshal the fields.
%%
%% @spec marshal(iso8583message()) -> binary()
-spec(marshal(iso8583message()) -> binary()).

marshal(Message) ->
	marshal(Message, erl8583_marshaller_binary_field).

%% @doc Marshals an ISO 8583 message into an ASCII string. This function
%%      uses the specified field marshalling module and the 
%%      erl8583_marshaller_binary_bitmap module to marshal the bit map.
%%
%% @spec marshal(iso8583message(), module()) -> binary()
-spec(marshal(iso8583message(), module()) -> binary()).

marshal(Message, FieldMarshaller) ->
	marshal(Message, FieldMarshaller, erl8583_marshaller_binary).

%% @doc Marshals an ISO 8583 message into an ASCII string. This function
%%      uses the specified field marshalling module and the 
%%      specified bit map marshaller.
%%
%% @spec marshal(iso8583message(), module(), module()) -> binary()
-spec(marshal(iso8583message(), module(), module()) -> binary()).

marshal(Message, FieldMarshaller, BitMapMarshaller) ->
	erl8583_marshaller:marshal(Message, [{field_marshaller, FieldMarshaller}, 
												{bitmap_marshaller, BitMapMarshaller}]).

%% @doc Unmarshals a binary into an ISO 8583 message. This function uses
%%      the erl8583_marshaller_binary_field module to unmarshal the fields.
%%
%% @spec unmarshal(list(byte())) -> iso8583message()
-spec(unmarshal(list(byte())) -> iso8583message()).

unmarshal(BinaryMessage) ->
	unmarshal(BinaryMessage, erl8583_marshaller_binary_field).

%% @doc Unmarshals a binary into an ISO 8583 message. This function uses
%%      the specified field marshalling module and the
%%      erl8583_marshaller_binary_bitmap module to unmarshal the
%%      bit map.
%%
%% @spec unmarshal(binary(), module()) -> iso8583message()
-spec(unmarshal(binary(), module()) -> iso8583message()).

unmarshal(BinaryMessage, FieldMarshaller) ->
	unmarshal(BinaryMessage, FieldMarshaller, erl8583_marshaller_binary).

%% @doc Unmarshals a binary into an ISO 8583 message. This function uses
%%      the specified field marshalling module and the specified
%%      bit map unmarshaller.
%%
%% @spec unmarshal(binary(), module(), module()) -> iso8583message()
-spec(unmarshal(binary(), module(), module()) -> iso8583message()).

unmarshal(BinaryMessage, FieldMarshaller, BitMapMarshaller) ->
	erl8583_marshaller:unmarshal(BinaryMessage, [{field_marshaller, FieldMarshaller},
												 {bitmap_marshaller, BitMapMarshaller}]).

%% @doc Constructs a binary representation of the
%%      bitmap for an iso8583message().
%%
%% @spec marshal_bitmap(list(integer())) -> list(byte())
-spec(marshal_bitmap(list(integer())) -> list(byte())).

marshal_bitmap([]) ->
	[];
marshal_bitmap(FieldIds) ->
	NumBitMaps = (lists:max(FieldIds) + 63) div 64,
	ExtensionBits = [Bit * 64 - 127 || Bit <- lists:seq(2, NumBitMaps)],
	BitMap = lists:duplicate(NumBitMaps * 8, 0),
	construct_bitmap(lists:sort(ExtensionBits ++ FieldIds), BitMap).

%% @doc Extracts a list of field IDs from a binary representation of 
%%      an ISO 8583 message.  The result is returned as a 2-tuple: a list
%%      of field IDs and the remainder of the message excluding the bit map.
%%
%% @spec unmarshal_bitmap(list(byte())) -> {list(integer()), list(byte())}
-spec(unmarshal_bitmap(list(byte())) -> {list(integer()), list(byte())}).

unmarshal_bitmap([]) ->
	{[], []};
unmarshal_bitmap(BinaryMessage) ->
	BitMapLength = get_bit_map_length(BinaryMessage),
	{BitMap, Fields} = lists:split(BitMapLength, BinaryMessage),
	{extract_fields(BitMap, 0, 8, []), Fields}.

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

get_bit_map_length(Message) ->
	[Head|_Tail] = Message,
	case Head >= 128 of
		false ->
			8;
		true ->
			{_, Rest} = lists:split(8, Message),
			8 + get_bit_map_length(Rest)
	end.

extract_fields([], _Offset, _Index, FieldIds) ->
	Ids = lists:sort(FieldIds),
	[Id || Id <- Ids, Id rem 64 =/= 1];
extract_fields([_Head|Tail], Offset, 0, FieldIds) ->
	extract_fields(Tail, Offset+1, 8, FieldIds);
extract_fields([Head|Tail], Offset, Index, FieldIds) ->
	case Head band (1 bsl (Index-1)) of
		0 ->
			extract_fields([Head|Tail], Offset, Index-1, FieldIds);
		_ ->
			extract_fields([Head|Tail], Offset, Index-1, [Offset*8+9-Index|FieldIds])
	end.
