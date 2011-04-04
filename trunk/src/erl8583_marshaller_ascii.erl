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
-export([marshal/1, marshal/2, marshal/3, unmarshal/1, unmarshal/2, unmarshal/3]).
-export([marshal_bitmap/1, unmarshal_bitmap/1]).

%%
%% API Functions
%%

%% @doc Marshals an ISO 8583 message into an ASCII string. This function
%%      uses the erl8583_marshaller_ascii_field module to marshal the fields
%%      and erl8385_marshaller_ascii_bitmap to marshal the bit map.
%%
%% @spec marshal(iso8583message()) -> string()
-spec(marshal(iso8583message()) -> string()).

marshal(Message) ->
	marshal(Message, erl8583_marshaller_ascii_field).

%% @doc Marshals an ISO 8583 message into an ASCII string. This function
%%      uses the specified field marshalling module and 
%%      erl8385_marshaller_ascii_bitmap to marshal the bit map.
%%
%% @spec marshal(iso8583message(), module()) -> string()
-spec(marshal(iso8583message(), module()) -> string()).

marshal(Message, FieldMarshaller) ->
	marshal(Message, FieldMarshaller, ?MODULE).
	
%% @doc Marshals an ISO 8583 message into an ASCII string. This function
%%      uses the specified field marshalling module and the specified
%%      bit map marshaller.
%%
%% @spec marshal(iso8583message(), module(), module()) -> string()
-spec(marshal(iso8583message(), module(), module()) -> string()).

marshal(Message, FieldMarshaller, BitMapMarshaller) ->
	erl8583_marshaller:marshal(Message, [{field_marshaller, FieldMarshaller}, 
										 {bitmap_marshaller, BitMapMarshaller}]).

%% @doc Unmarshals an ASCII string into an ISO 8583 message. This function
%%      uses the erl8583_marshaller_ascii_field module to unmarshal the fields
%%      and the erl8583_marshaller_ascii_bitmap module to unmarshal the bit map.
%%
%% @spec unmarshal(string()) -> iso8583message()
-spec(unmarshal(string()) -> iso8583message()).

unmarshal(AsciiMessage) ->
	unmarshal(AsciiMessage, erl8583_marshaller_ascii_field).

%% @doc Unmarshals an ASCII string into an ISO 8583 message. This function
%%      uses the specified field marshalling module and the 
%%      erl8583_marshaller_ascii_bitmap module to unmarshal the bit map.
%%
%% @spec unmarshal(string(), module()) -> iso8583message()
-spec(unmarshal(string(), module()) -> iso8583message()).

unmarshal(AsciiMessage, FieldMarshaller) ->
	unmarshal(AsciiMessage, FieldMarshaller, ?MODULE).

%% @doc Unmarshals an ASCII string into an ISO 8583 message. This function
%%      uses the specified field marshalling module and the 
%%      specified module to unmarshal the bit map.
%%
%% @spec unmarshal(string(), module(), module()) -> iso8583message()
-spec(unmarshal(string(), module(), module()) -> iso8583message()).

unmarshal(AsciiMessage, FieldMarshaller, BitMapMarshaller) ->
	erl8583_marshaller:unmarshal(AsciiMessage, [{field_marshaller, FieldMarshaller}, 
												{bitmap_marshaller, BitMapMarshaller}]).

%% @doc Constructs an ASCII string representation of the
%%      bitmap for an iso8583message().
%%
%% @spec marshal_bitmap(list(integer())) -> string()
-spec(marshal_bitmap(list(integer())) -> string()).

marshal_bitmap([]) ->
	[];
marshal_bitmap(FieldIds) ->
	NumBitMaps = (lists:max(FieldIds) + 63) div 64,
	ExtensionBits = [Bit * 64 - 127 || Bit <- lists:seq(2, NumBitMaps)],
	BitMap = lists:duplicate(NumBitMaps * 8, 0),
	erl8583_convert:string_to_ascii_hex(construct_bitmap(lists:sort(ExtensionBits ++ FieldIds), BitMap)).

%% @doc Extracts a list of field IDs from an ASCII string 
%%      representation of an ISO 8583 message. The result is returned
%%      as a 2-tuple of the field IDs and the remainder of the 
%%      the message (encoding the field values but not the bit map).
%%
%% @spec unmarshal_bitmap(string()) -> {list(integer()), string()}
-spec(unmarshal_bitmap(string()) -> {list(integer()), string()}).

unmarshal_bitmap([]) ->
	{[], []};
unmarshal_bitmap(AsciiMessage) ->
	BitMapLength = get_bit_map_length(AsciiMessage),
	{AsciiBitMap, Fields} = lists:split(BitMapLength, AsciiMessage),
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
