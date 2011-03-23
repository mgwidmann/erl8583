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
%% @doc marshaller_binary_field. This module marshalls a field of an iso8583message 
%%      into a binary representation.

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

%% @doc Marshals a data element into a binary given the field encoding
%%      and the value of the data element.
%%
%% @spec marshal_data_element(field_encoding(), iso8583field_value()) -> string()
-spec(marshal_data_element(field_encoding(), iso8583field_value()) -> string()).

marshal_data_element({n, llvar, Length}, Value) when length(Value) =< Length ->
	LField = erl8583_convert:integer_to_bcd(length(Value), 2),
	VField = erl8583_convert:ascii_hex_to_bcd(Value, "0"),
	erl8583_convert:concat_binaries(LField, VField);
marshal_data_element({z, llvar, Length}, Value) when length(Value) =< Length ->
	LField = erl8583_convert:integer_to_bcd(length(Value), 2),
	VField = erl8583_convert:string_to_track2(Value),
	erl8583_convert:concat_binaries(LField, VField);
marshal_data_element({n, fixed, Length}, Value) ->
	case Length rem 2 of
		0 ->
			PaddedValue = erl8583_convert:integer_to_string(list_to_integer(Value), Length);
		1 ->
			PaddedValue = erl8583_convert:integer_to_string(list_to_integer(Value), Length+1)
	end,
	erl8583_convert:ascii_hex_to_bcd(PaddedValue, "0");
marshal_data_element({an, fixed, Length}, Value) ->
	list_to_binary(erl8583_convert:pad_with_trailing_spaces(Value, Length));
marshal_data_element({ans, fixed, Length}, Value) ->
	list_to_binary(erl8583_convert:pad_with_trailing_spaces(Value, Length));
marshal_data_element({an, llvar, Length}, Value) when length(Value) =< Length ->
	LField = erl8583_convert:integer_to_bcd(length(Value), 2),
	erl8583_convert:concat_binaries(LField, list_to_binary(Value));
marshal_data_element({ns, llvar, Length}, Value) when length(Value) =< Length ->
	LField = erl8583_convert:integer_to_bcd(length(Value), 2),
	erl8583_convert:concat_binaries(LField, list_to_binary(Value));
marshal_data_element({ans, llvar, Length}, Value) when length(Value) =< Length ->
	LField = erl8583_convert:integer_to_bcd(length(Value), 2),
	erl8583_convert:concat_binaries(LField, list_to_binary(Value));
marshal_data_element({ans, lllvar, Length}, Value) when length(Value) =< Length ->
	LField = erl8583_convert:integer_to_bcd(length(Value), 3),
	erl8583_convert:concat_binaries(LField, list_to_binary(Value));
marshal_data_element({x_n, fixed, Length}, [Head | Value]) when Head =:= $C orelse Head =:= $D ->
	IntValue = list_to_integer(Value),
	erl8583_convert:concat_binaries(<<Head>>,  erl8583_convert:integer_to_bcd(IntValue, Length));
marshal_data_element({b, fixed, Length}, Value) when size(Value) =:= Length ->
	Value.

%% @doc Extracts a field value from the start of a binary given how the field
%%      is encoded.  The field value and the rest of the unmarshalled binary
%%      is returned as a tuple.
%%
%% @spec unmarshal_data_element(field_encoding(), binary()) -> {iso8583field_value(), binary()}
-spec(unmarshal_data_element(field_encoding(), binary()) -> {iso8583field_value(), binary()}).

unmarshal_data_element({n, llvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 1),
	N = erl8583_convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, (N+1) div 2), 
	{erl8583_convert:bcd_to_ascii_hex(ValueBin, N, "0"), Rest};
unmarshal_data_element({n, lllvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 2),
	N = erl8583_convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, (N+1) div 2), 
	{erl8583_convert:bcd_to_ascii_hex(ValueBin, N, "0"), Rest};
unmarshal_data_element({an, llvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 1),
	N = erl8583_convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, N), 
	{binary_to_list(ValueBin), Rest};
unmarshal_data_element({ns, llvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 1),
	N = erl8583_convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, N), 
	{binary_to_list(ValueBin), Rest};
unmarshal_data_element({ans, llvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 1),
	N = erl8583_convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, N), 
	{binary_to_list(ValueBin), Rest};
unmarshal_data_element({ans, lllvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 2),
	N = erl8583_convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, N), 
	{binary_to_list(ValueBin), Rest};
unmarshal_data_element({n, fixed, Length}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, (Length + 1) div 2),
	case Length rem 2 of
		0 ->
			{erl8583_convert:bcd_to_ascii_hex(NBin, Length, "0"), RestBin};
		1 ->
			[$0|AsciiHex] = erl8583_convert:bcd_to_ascii_hex(NBin, Length+1, "0"),
			{AsciiHex, RestBin}
	end;
unmarshal_data_element({an, fixed, Length}, Fields) ->
	{FieldBin, RestBin} = split_binary(Fields, Length),
	FieldStr = binary_to_list(FieldBin),
	{erl8583_convert:pad_with_trailing_spaces(FieldStr, Length), RestBin};
unmarshal_data_element({ans, fixed, Length}, Fields) ->
	{FieldBin, RestBin} = split_binary(Fields, Length),
	FieldStr = binary_to_list(FieldBin),
	{erl8583_convert:pad_with_trailing_spaces(FieldStr, Length), RestBin};
unmarshal_data_element({x_n, fixed, Length}, Fields) ->
	{FieldBin, RestBin} = split_binary(Fields, Length div 2 + 1),
	{<<X>>, Value} = split_binary(FieldBin, 1),
	ValueStr = erl8583_convert:bcd_to_ascii_hex(Value, Length, "0"),
	case X =:= $C orelse X =:= $D of
		true ->
			{[X] ++ ValueStr, RestBin}
	end;
unmarshal_data_element({z, llvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 1),
	N = erl8583_convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, (N+1) div 2), 
	{erl8583_convert:track2_to_string(ValueBin, N), Rest};
unmarshal_data_element({b, fixed, Length}, Fields) ->
	split_binary(Fields, Length).

%% @doc Marshals a field value into an ASCII string.
%%
%% @spec marshal(integer(), iso8583field_value()) -> binary()
-spec(marshal(integer(), iso8583field_value()) -> binary()).

marshal(FieldId, Value) ->
	Pattern = erl8583_fields:get_encoding(FieldId),
	marshal_data_element(Pattern, Value).

%% @doc Extracts a field value from the start of a string.  The field value 
%%      and the rest of the unmarshalled string is returned as a tuple.
%%
%% @spec unmarshal(integer(), binary()) -> {iso8583field_value(), binary()}
-spec(unmarshal(integer(), binary()) -> {iso8583field_value(), binary()}).

unmarshal(FieldId, Fields) ->
	Pattern = erl8583_fields:get_encoding(FieldId),
	unmarshal_data_element(Pattern, Fields).


%%
%% Local Functions
%%

