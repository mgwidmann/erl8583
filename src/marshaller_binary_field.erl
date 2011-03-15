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
%%      into an ASCII hex string.

-module(marshaller_binary_field).

%%
%% Include files
%%
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
-spec(marshal_data_element(field_encoding(), iso8583field_value()) -> string()).

marshal_data_element({n, llvar, Length}, Value) when length(Value) =< Length ->
	LField = convert:integer_to_bcd(length(Value), 2),
	VField = convert:ascii_hex_to_bcd(Value, "0"),
	convert:concat_binaries(LField, VField);
marshal_data_element({z, llvar, Length}, Value) when length(Value) =< Length ->
	LField = convert:integer_to_bcd(length(Value), 2),
	VField = convert:string_to_track2(Value),
	convert:concat_binaries(LField, VField);
marshal_data_element({n, fixed, Length}, Value) ->
	case Length rem 2 of
		0 ->
			PaddedValue = convert:integer_to_string(list_to_integer(Value), Length);
		1 ->
			PaddedValue = convert:integer_to_string(list_to_integer(Value), Length+1)
	end,
	convert:ascii_hex_to_bcd(PaddedValue, "0");
marshal_data_element({an, fixed, Length}, Value) ->
	list_to_binary(convert:pad_with_trailing_spaces(Value, Length));
marshal_data_element({ans, fixed, Length}, Value) ->
	list_to_binary(convert:pad_with_trailing_spaces(Value, Length));
marshal_data_element({an, llvar, Length}, Value) when length(Value) =< Length ->
	LField = convert:integer_to_bcd(length(Value), 2),
	convert:concat_binaries(LField, list_to_binary(Value));
marshal_data_element({ns, llvar, Length}, Value) when length(Value) =< Length ->
	LField = convert:integer_to_bcd(length(Value), 2),
	convert:concat_binaries(LField, list_to_binary(Value));
marshal_data_element({ans, llvar, Length}, Value) when length(Value) =< Length ->
	LField = convert:integer_to_bcd(length(Value), 2),
	convert:concat_binaries(LField, list_to_binary(Value));
marshal_data_element({ans, lllvar, Length}, Value) when length(Value) =< Length ->
	LField = convert:integer_to_bcd(length(Value), 3),
	convert:concat_binaries(LField, list_to_binary(Value));
marshal_data_element({x_n, fixed, Length}, [Head | Value]) when Head =:= $C orelse Head =:= $D ->
	IntValue = list_to_integer(Value),
	convert:concat_binaries(<<Head>>,  convert:integer_to_bcd(IntValue, Length));
marshal_data_element({b, Length}, Value) when size(Value) =:= Length ->
	Value.

%% @doc Extracts a field value from the start of a binary given how the field
%%      is encoded.  The field value and the rest of the unmarshalled binary
%%      is returned as a tuple.
-spec(unmarshal_data_element(field_encoding(), binary()) -> {iso8583field_value(), binary()}).

unmarshal_data_element({n, llvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 1),
	N = convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, (N+1) div 2), 
	{convert:bcd_to_ascii_hex(ValueBin, N, "0"), Rest};
unmarshal_data_element({n, lllvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 2),
	N = convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, (N+1) div 2), 
	{convert:bcd_to_ascii_hex(ValueBin, N, "0"), Rest};
unmarshal_data_element({an, llvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 1),
	N = convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, N), 
	{binary_to_list(ValueBin), Rest};
unmarshal_data_element({ns, llvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 1),
	N = convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, N), 
	{binary_to_list(ValueBin), Rest};
unmarshal_data_element({ans, llvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 1),
	N = convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, N), 
	{binary_to_list(ValueBin), Rest};
unmarshal_data_element({ans, lllvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 2),
	N = convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, N), 
	{binary_to_list(ValueBin), Rest};
unmarshal_data_element({n, fixed, Length}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, (Length + 1) div 2),
	case Length rem 2 of
		0 ->
			{convert:bcd_to_ascii_hex(NBin, Length, "0"), RestBin};
		1 ->
			[$0|AsciiHex] = convert:bcd_to_ascii_hex(NBin, Length+1, "0"),
			{AsciiHex, RestBin}
	end;
unmarshal_data_element({an, fixed, Length}, Fields) ->
	{FieldBin, RestBin} = split_binary(Fields, Length),
	FieldStr = binary_to_list(FieldBin),
	{convert:pad_with_trailing_spaces(FieldStr, Length), RestBin};
unmarshal_data_element({ans, fixed, Length}, Fields) ->
	{FieldBin, RestBin} = split_binary(Fields, Length),
	FieldStr = binary_to_list(FieldBin),
	{convert:pad_with_trailing_spaces(FieldStr, Length), RestBin};
unmarshal_data_element({x_n, fixed, Length}, Fields) ->
	{FieldBin, RestBin} = split_binary(Fields, Length div 2 + 1),
	{<<X>>, Value} = split_binary(FieldBin, 1),
	ValueStr = convert:bcd_to_ascii_hex(Value, Length, "0"),
	case X =:= $C orelse X =:= $D of
		true ->
			{[X] ++ ValueStr, RestBin}
	end;
unmarshal_data_element({z, llvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 1),
	N = convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, (N+1) div 2), 
	{convert:track2_to_string(ValueBin, N), Rest};
unmarshal_data_element({b, Length}, Fields) ->
	split_binary(Fields, Length).

%% @doc Marshals a field value into an ASCII string.
-spec(marshal(integer(), iso8583field_value()) -> binary()).

marshal(FieldId, Value) ->
	Pattern = iso8583_fields:get_encoding(FieldId),
	marshal_data_element(Pattern, Value).

%% @doc Extracts a field value from the start of a string.  The field value 
%%      and the rest of the unmarshalled string is returned as a tuple.
-spec(unmarshal(integer(), binary()) -> {iso8583field_value(), binary()}).

unmarshal(FieldId, Fields) ->
	Pattern = iso8583_fields:get_encoding(FieldId),
	unmarshal_data_element(Pattern, Fields).


%%
%% Local Functions
%%

