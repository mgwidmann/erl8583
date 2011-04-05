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
-export([marshal_bitmap/1, unmarshal_bitmap/1]).
-export([marshal_field/3, unmarshal_field/3]).
-export([marshal_mti/1]).


%%
%% API Functions
%%

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

%% @doc Marshals a field value into a binary. The 1987 version
%%      of the ISO 8583 specification is used to determine how to
%%      encode the field value.
%%
%% @spec marshal_field(integer(), iso8583field_value(), module()) -> binary()
-spec(marshal_field(integer(), iso8583field_value(), module()) -> binary()).

marshal_field(FieldId, FieldValue, EncodingRules) ->
	Pattern = EncodingRules:get_encoding(FieldId),
	marshal_data_element(Pattern, FieldValue).

marshal_mti(Mti) ->
	marshal_field(0, Mti, erl8583_fields).

%% @doc Extracts a field value from the start of a binary.  The field value 
%%      and the rest of the unmarshalled binary are returned as a 2-tuple.
%%      The 1987 version of the ISO 8583 specification is used to determine how to
%%      decode the field value.
%%
%% @spec unmarshal_field(integer(), list(byte()), module()) -> {iso8583field_value(), binary()}
-spec(unmarshal_field(integer(), list(byte()), module()) -> {iso8583field_value(), binary()}).

unmarshal_field(FieldId, BinaryFields, EncodingRules) ->
	Pattern = EncodingRules:get_encoding(FieldId),
	unmarshal_data_element(Pattern, BinaryFields).

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

marshal_data_element({n, llvar, Length}, FieldValue) when length(FieldValue) =< Length ->
	LField = erl8583_convert:integer_to_bcd(length(FieldValue), 2),
	VField = erl8583_convert:ascii_hex_to_bcd(FieldValue, "0"),
	LField ++ VField;
marshal_data_element({z, llvar, Length}, FieldValue) when length(FieldValue) =< Length ->
	LField = erl8583_convert:integer_to_bcd(length(FieldValue), 2),
	VField = erl8583_convert:string_to_track2(FieldValue),
	LField ++ VField;
marshal_data_element({n, fixed, Length}, FieldValue) ->
	case Length rem 2 of
		0 ->
			PaddedValue = erl8583_convert:integer_to_string(list_to_integer(FieldValue), Length);
		1 ->
			PaddedValue = erl8583_convert:integer_to_string(list_to_integer(FieldValue), Length+1)
	end,
	erl8583_convert:ascii_hex_to_bcd(PaddedValue, "0");
marshal_data_element({an, fixed, Length}, FieldValue) ->
	erl8583_convert:pad_with_trailing_spaces(FieldValue, Length);
marshal_data_element({ans, fixed, Length}, FieldValue) ->
	erl8583_convert:pad_with_trailing_spaces(FieldValue, Length);
marshal_data_element({an, llvar, Length}, FieldValue) when length(FieldValue) =< Length ->
	LField = erl8583_convert:integer_to_bcd(length(FieldValue), 2),
	LField ++ FieldValue;
marshal_data_element({ns, llvar, Length}, FieldValue) when length(FieldValue) =< Length ->
	LField = erl8583_convert:integer_to_bcd(length(FieldValue), 2),
	LField ++ FieldValue;
marshal_data_element({ans, llvar, Length}, FieldValue) when length(FieldValue) =< Length ->
	LField = erl8583_convert:integer_to_bcd(length(FieldValue), 2),
	LField ++ FieldValue;
marshal_data_element({ans, lllvar, Length}, FieldValue) when length(FieldValue) =< Length ->
	LField = erl8583_convert:integer_to_bcd(length(FieldValue), 3),
	LField ++ FieldValue;
marshal_data_element({x_n, fixed, Length}, [Head | FieldValue]) when Head =:= $C orelse Head =:= $D ->
	IntValue = list_to_integer(FieldValue),
	[Head|erl8583_convert:integer_to_bcd(IntValue, Length)];
marshal_data_element({b, fixed, Length}, FieldValue) when size(FieldValue) =:= Length ->
	binary_to_list(FieldValue).

unmarshal_data_element({n, llvar, _MaxLength}, BinaryFields) ->
	[NBin|RestBin] = BinaryFields,
	N = erl8583_convert:bcd_to_integer([NBin]),
	{ValueBin, Rest} = lists:split((N+1) div 2, RestBin), 
	{erl8583_convert:bcd_to_ascii_hex(ValueBin, N, "0"), Rest};
unmarshal_data_element({n, lllvar, _MaxLength}, BinaryFields) ->
	{NBin, RestBin} = lists:split(2, BinaryFields),
	N = erl8583_convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = lists:split((N+1) div 2, RestBin), 
	{erl8583_convert:bcd_to_ascii_hex(ValueBin, N, "0"), Rest};
unmarshal_data_element({an, llvar, _MaxLength}, BinaryFields) ->
	[NBin|RestBin] = BinaryFields,
	N = erl8583_convert:bcd_to_integer([NBin]),
	lists:split(N, RestBin); 
unmarshal_data_element({ns, llvar, _MaxLength}, BinaryFields) ->
	[NBin|Rest] = BinaryFields,
	N = erl8583_convert:bcd_to_integer([NBin]),
	lists:split(N, Rest); 
unmarshal_data_element({ans, llvar, _MaxLength}, BinaryFields) ->
	[NBin|Rest] = BinaryFields,
	N = erl8583_convert:bcd_to_integer([NBin]),
	lists:split(N, Rest); 
unmarshal_data_element({ans, lllvar, _MaxLength}, BinaryFields) ->
	{NBin, Rest} = lists:split(2, BinaryFields),
	N = erl8583_convert:bcd_to_integer(NBin),
	lists:split(N, Rest); 
unmarshal_data_element({n, fixed, Length}, BinaryFields) ->
	{NBin, RestBin} = lists:split((Length + 1) div 2, BinaryFields),
	case Length rem 2 of
		0 ->
			{erl8583_convert:bcd_to_ascii_hex(NBin, Length, "0"), RestBin};
		1 ->
			[$0|AsciiHex] = erl8583_convert:bcd_to_ascii_hex(NBin, Length+1, "0"),
			{AsciiHex, RestBin}
	end;
unmarshal_data_element({an, fixed, Length}, BinaryFields) ->
	{Field, Rest} = lists:split(Length, BinaryFields),
	{erl8583_convert:pad_with_trailing_spaces(Field, Length), Rest};
unmarshal_data_element({ans, fixed, Length}, BinaryFields) ->
	{Field, Rest} = lists:split(Length, BinaryFields),
	{erl8583_convert:pad_with_trailing_spaces(Field, Length), Rest};
unmarshal_data_element({x_n, fixed, Length}, BinaryFields) ->
	{Field, Rest} = lists:split(Length div 2 + 1, BinaryFields),
	{[X], Value} = lists:split(1, Field),
	ValueStr = erl8583_convert:bcd_to_ascii_hex(Value, Length, "0"),
	case X =:= $C orelse X =:= $D of
		true ->
			{[X] ++ ValueStr, Rest}
	end;
unmarshal_data_element({z, llvar, _MaxLength}, BinaryFields) ->
	[NBin|RestBin] = BinaryFields,
	N = erl8583_convert:bcd_to_integer([NBin]),
	{ValueBin, Rest} = lists:split((N+1) div 2, RestBin), 
	{erl8583_convert:track2_to_string(ValueBin, N), Rest};
unmarshal_data_element({b, fixed, Length}, BinaryFields) ->
	{Bin, Rest} = lists:split(Length, BinaryFields),
	{list_to_binary(Bin), Rest}.
