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
%% @doc This module marshals an iso8583message() field into 
%%      an ASCII string or unmarshals an ASCII string into
%%      an iso8583message() field.

-module(erl8583_marshaller_ascii_field).

%%
%% Include files
%%
%% @headerfile "../include/erl8583_types.hrl"
-include("erl8583_types.hrl").

%%
%% Exported Functions
%%
-export([marshal_field/2, marshal_data_element/2, unmarshal_field/2, unmarshal_data_element/2]).

%%
%% API Functions
%%

%% @doc Marshals a data element into a string given the field encoding
%%      and the value of the data element.
%%
%% @spec marshal_data_element(Encoding::field_encoding(), iso8583field_value()) -> string()
-spec(marshal_data_element(field_encoding(), iso8583field_value()) -> string()).

marshal_data_element({n, llvar, Length}, FieldValue) when length(FieldValue) =< Length ->
	erl8583_convert:integer_to_string(length(FieldValue), 2) ++ FieldValue;
marshal_data_element({n, lllvar, Length}, FieldValue) when length(FieldValue) =< Length ->
	erl8583_convert:integer_to_string(length(FieldValue), 3) ++ FieldValue;
marshal_data_element({ns, llvar, Length}, FieldValue) when length(FieldValue) =< Length ->
	erl8583_convert:integer_to_string(length(FieldValue), 2) ++ FieldValue;
marshal_data_element({an, llvar, Length}, FieldValue) when length(FieldValue) =< Length ->
	erl8583_convert:integer_to_string(length(FieldValue), 2) ++ FieldValue;
marshal_data_element({an, lllvar, Length}, FieldValue) when length(FieldValue) =< Length ->
	erl8583_convert:integer_to_string(length(FieldValue), 3) ++ FieldValue;
marshal_data_element({ans, llvar, Length}, FieldValue) when length(FieldValue) =< Length ->
	erl8583_convert:integer_to_string(length(FieldValue), 2) ++ FieldValue;
marshal_data_element({ans, lllvar, Length}, FieldValue) when length(FieldValue) =< Length ->
	erl8583_convert:integer_to_string(length(FieldValue), 3) ++ FieldValue;
marshal_data_element({n, fixed, Length}, FieldValue) when length(FieldValue) =< Length ->
	IntValue = list_to_integer(FieldValue),
	erl8583_convert:integer_to_string(IntValue, Length);
marshal_data_element({an, fixed, Length}, FieldValue) when length(FieldValue) =< Length ->
	erl8583_convert:pad_with_trailing_spaces(FieldValue, Length);
marshal_data_element({ans, fixed, Length}, FieldValue) when length(FieldValue) =< Length ->
	erl8583_convert:pad_with_trailing_spaces(FieldValue, Length);
marshal_data_element({x_n, fixed, Length}, [Head | FieldValue]) when Head =:= $C orelse Head =:= $D ->
	IntValue = list_to_integer(FieldValue),
	[Head] ++ erl8583_convert:integer_to_string(IntValue, Length);
marshal_data_element({z, llvar, Length}, FieldValue) when length(FieldValue) =< Length ->
	erl8583_convert:integer_to_string(length(FieldValue), 2) ++ FieldValue;
marshal_data_element({b, fixed, Length}, FieldValue) when size(FieldValue) =:= Length ->
	erl8583_convert:binary_to_ascii_hex(FieldValue).

%% @doc Extracts a field value from the start of a string given how the field
%%      is encoded.  The field value and the rest of the unmarshalled string
%%      is returned as a 2-tuple.
%%
%% @spec unmarshal_data_element(Encoding::field_encoding(), string()) -> {iso8583field_value(), string()}
-spec(unmarshal_data_element(field_encoding(), string()) -> {iso8583field_value(), string()}).

unmarshal_data_element({n, llvar, _MaxLength}, AsciiFields) ->
	{N, Rest} = lists:split(2, AsciiFields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({n, lllvar, _MaxLength}, AsciiFields) ->
	{N, Rest} = lists:split(3, AsciiFields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({ns, llvar, _MaxLength}, AsciiFields) ->
	{N, Rest} = lists:split(2, AsciiFields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({an, llvar, _MaxLength}, AsciiFields) ->
	{N, Rest} = lists:split(2, AsciiFields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({an, lllvar, _MaxLength}, AsciiFields) ->
	{N, Rest} = lists:split(3, AsciiFields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({ans, llvar, _MaxLength}, AsciiFields) ->
	{N, Rest} = lists:split(2, AsciiFields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({ans, lllvar, _MaxLength}, AsciiFields) ->
	{N, Rest} = lists:split(3, AsciiFields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({n, fixed, Length}, AsciiFields) ->
	lists:split(Length, AsciiFields);
unmarshal_data_element({an, fixed, Length}, AsciiFields) ->
	lists:split(Length, AsciiFields);
unmarshal_data_element({ans, fixed, Length}, AsciiFields) ->
	lists:split(Length, AsciiFields);
unmarshal_data_element({x_n, fixed, Length}, [Head|Tail]) when Head =:= $C orelse Head =:= $D ->
	lists:split(Length+1, [Head|Tail]);
unmarshal_data_element({z, llvar, _MaxLength}, AsciiFields) ->
	{N, Rest} = lists:split(2, AsciiFields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({b, fixed, Length}, AsciiFields) ->
	{ValueStr, Rest} = lists:split(2 * Length, AsciiFields),
	Value = erl8583_convert:ascii_hex_to_binary(ValueStr),
	{Value, Rest}.

%% @doc Marshals a field value into an ASCII string. The 1987 version
%%      of the ISO 8583 specification is used to determine how to
%%      encode the field value.
%%
%% @spec marshal_field(integer(), iso8583field_value()) -> string()
-spec(marshal_field(integer(), iso8583field_value()) -> string()).

marshal_field(FieldId, FieldValue) ->
	Pattern = erl8583_fields:get_encoding(FieldId),
	marshal_data_element(Pattern, FieldValue).

%% @doc Extracts a field value from the start of a string.  The field value 
%%      and the rest of the unmarshalled string is returned as a 2-tuple.
%%      The 1987 version of the ISO 8583 specification is used to determine how to
%%      decode the field value.
%%
%% @spec unmarshal_field(integer(), string()) -> {iso8583field_value(), string()}
-spec(unmarshal_field(integer(), string()) -> {iso8583field_value(), string()}).

unmarshal_field(FieldId, AsciiFields) ->
	Pattern = erl8583_fields:get_encoding(FieldId),
	unmarshal_data_element(Pattern, AsciiFields).


%%
%% Local Functions
%%

