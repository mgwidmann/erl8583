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
%% @doc This module marshals a field of an iso8583message() 
%%      into a binary representation or unmarshals a binary
%%      value into a field of an iso8583message().

-module(erl8583_marshaller_binary_field).

%%
%% Include files
%%
%% @headerfile "../include/erl8583_types.hrl"
-include("erl8583_types.hrl").

%%
%% Exported Functions
%%
-export([marshal/2, marshal_data_element/2, unmarshal/2, unmarshal_data_element/2]).

%%
%% API Functions
%%

%% @doc Marshals a data element into a binary value given the field encoding
%%      and the value of the data element.
%%
%% @spec marshal_data_element(Encoding::field_encoding(), iso8583field_value()) -> list(byte())
-spec(marshal_data_element(field_encoding(), iso8583field_value()) -> list(byte())).

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

%% @doc Extracts a field value from the start of a binary given how the field
%%      is encoded.  The field value and the rest of the unmarshalled binary
%%      is returned as a 2-tuple.
%%
%% @spec unmarshal_data_element(Encoding::field_encoding(), binary()) -> {iso8583field_value(), binary()}
-spec(unmarshal_data_element(field_encoding(), binary()) -> {iso8583field_value(), binary()}).

unmarshal_data_element({n, llvar, _MaxLength}, BinaryFields) ->
	{NBin, RestBin} = lists:split(1, BinaryFields),
	N = erl8583_convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = lists:split((N+1) div 2, RestBin), 
	{erl8583_convert:bcd_to_ascii_hex(ValueBin, N, "0"), Rest};
unmarshal_data_element({n, lllvar, _MaxLength}, BinaryFields) ->
	{NBin, RestBin} = split_binary(BinaryFields, 2),
	N = erl8583_convert:bcd_to_integer(binary_to_list(NBin)),
	{ValueBin, Rest} = split_binary(RestBin, (N+1) div 2), 
	{erl8583_convert:bcd_to_ascii_hex(binary_to_list(ValueBin), N, "0"), Rest};
unmarshal_data_element({an, llvar, _MaxLength}, BinaryFields) ->
	{NBin, RestBin} = lists:split(1, BinaryFields),
	N = erl8583_convert:bcd_to_integer(NBin),
	lists:split(N, RestBin); 
	%{binary_to_list(ValueBin, Rest};
unmarshal_data_element({ns, llvar, _MaxLength}, BinaryFields) ->
	{NBin, Rest} = lists:split(1, BinaryFields),
	N = erl8583_convert:bcd_to_integer(NBin),
	lists:split(N, Rest); 
unmarshal_data_element({ans, llvar, _MaxLength}, BinaryFields) ->
	{NBin, Rest} = lists:split(1, BinaryFields),
	N = erl8583_convert:bcd_to_integer(NBin),
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
	{NBin, RestBin} = lists:split(1, BinaryFields),
	N = erl8583_convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = lists:split((N+1) div 2, RestBin), 
	{erl8583_convert:track2_to_string(ValueBin, N), Rest};
unmarshal_data_element({b, fixed, Length}, BinaryFields) ->
	{Bin, Rest} = lists:split(Length, BinaryFields),
	{list_to_binary(Bin), Rest}.

%% @doc Marshals a field value into a binary. The 1987 version
%%      of the ISO 8583 specification is used to determine how to
%%      encode the field value.
%%
%% @spec marshal(integer(), iso8583field_value()) -> binary()
-spec(marshal(integer(), iso8583field_value()) -> binary()).

marshal(FieldId, FieldValue) ->
	Pattern = erl8583_fields:get_encoding(FieldId),
	marshal_data_element(Pattern, FieldValue).

%% @doc Extracts a field value from the start of a binary.  The field value 
%%      and the rest of the unmarshalled binary are returned as a 2-tuple.
%%      The 1987 version of the ISO 8583 specification is used to determine how to
%%      decode the field value.
%%
%% @spec unmarshal(integer(), list(byte())) -> {iso8583field_value(), binary()}
-spec(unmarshal(integer(), list(byte())) -> {iso8583field_value(), binary()}).

unmarshal(FieldId, BinaryFields) ->
	Pattern = erl8583_fields:get_encoding(FieldId),
	unmarshal_data_element(Pattern, BinaryFields).


%%
%% Local Functions
%%

