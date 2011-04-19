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
%% @doc This module provides functions to marshal an iso8583message() 
%%      into a list of bytes, to unmarshal a list of bytes into an
%%      iso8583message() and to marshal/unmarshal the MTI, bitmap
%%      and fields of an ISO 8583 message.
%%      
%%      When functions are used to unmarshal the MTI, bitmap or a 
%%      field of a message, the function assumes that the component
%%      to be unmarshalled is at the start of the list. The
%%      returned value is a 2-tuple containing the unmarshalled value
%%      and the remainder of the list that needs to be unmarshalled.
-module(erl8583_marshaller_binary).

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
		 marshal_bitmap/1, 
		 unmarshal_bitmap/1,
		 marshal_field/3, 
		 unmarshal_field/3,
		 marshal_mti/1, 
		 unmarshal_mti/1]).


%%
%% API Functions
%%

%% @doc Marshals an iso8583message() to a list of bytes.
%%
%% @spec marshal(iso8583message()) -> list(byte())
-spec(marshal(iso8583message()) -> list(byte())).

marshal(Message) ->
	erl8583_marshaller:marshal(Message, ?MARSHALLER_BINARY).

%% @doc Unmarshals a list of bytes to an iso8583message().
%%
%% @spec unmarshal(list(byte())) -> iso8583message()
-spec(unmarshal(list(byte())) -> iso8583message()).

unmarshal(Marshalled) ->
	erl8583_marshaller:unmarshal(Marshalled, ?MARSHALLER_BINARY).
	
%% @doc Constructs a list of bytes representation of the
%%      bitmap for an iso8583message().
%%
%% @spec marshal_bitmap(list(integer())) -> list(byte())
-spec(marshal_bitmap(list(integer())) -> list(byte())).

marshal_bitmap(Message) ->
	FieldIds = erl8583_message:get_fields(Message) -- [0],
	FieldIds = erl8583_message:get_fields(Message) -- [0],
	case lists:max(FieldIds) > 64 of
		true ->
			PrimaryFields = [1] ++ [Field || Field <- FieldIds, Field =< 64],
			SecondaryFields = [Field-64 || Field <- FieldIds, Field > 64],
			SecondaryBitmap = erl8583_convert:list_to_bitmap(SecondaryFields),
			UpdatedMessage = erl8583_message:set(1, SecondaryBitmap, Message);
		false ->
			PrimaryFields = FieldIds,
			UpdatedMessage = Message
	end,
	{binary_to_list(erl8583_convert:list_to_bitmap(PrimaryFields)), UpdatedMessage}.

%% @doc Extracts a list of field IDs from a list of bytes representation of 
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

%% @doc Marshals a field value into a byte list using a specified
%%      encoding rules module.
%%
%% @spec marshal_field(integer(), iso8583field_value(), module()) -> list(byte())
-spec(marshal_field(integer(), iso8583field_value(), module()) -> list(byte())).

marshal_field(FieldId, FieldValue, EncodingRules) ->
	Pattern = EncodingRules:get_encoding(FieldId),
	marshal_data_element(Pattern, FieldValue).

%% @doc Extracts a field value from the start of a byte list.  The field value 
%%      and the rest of the unmarshalled byte list is returned as a 2-tuple.
%%      A module that specifies how the field is encoded must be passed
%%      as an argument.
%%
%% @spec unmarshal_field(integer(), list(byte()), module()) -> {iso8583field_value(), list(byte())}
-spec(unmarshal_field(integer(), list(byte()), module()) -> {iso8583field_value(), list(byte())}).

unmarshal_field(FieldId, BinaryFields, EncodingRules) ->
	Pattern = EncodingRules:get_encoding(FieldId),
	case unmarshal_data_element(Pattern, BinaryFields) of
		{Value, Rest} ->
			{Value, Rest, []}
	end.

%% @doc Marshals the MTI into a byte list.
%%
%% @spec marshal_mti(string()) -> list(byte())
-spec(marshal_mti(string()) -> list(byte())).

marshal_mti(Mti) ->
	marshal_field(0, Mti, erl8583_fields).

%% @doc Extracts the MTI from the start of a byte list.  The MTI 
%%      and the rest of the unmarshalled list is returned as a 2-tuple.
%%
%% @spec unmarshal_mti(list(byte())) -> {string(), list(byte())}
-spec(unmarshal_mti(list(byte())) -> {string(), list(byte())}).

unmarshal_mti(Marshalled) ->
	{Value, Rest, []} = unmarshal_field(0, Marshalled, erl8583_fields),
	{Value, Rest}.


%%
%% Local Functions
%%
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
	binary_to_list(FieldValue);
marshal_data_element({bitmap, fixed, Length}, FieldValue) when length(FieldValue) =:= Length ->
	FieldValue.

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
