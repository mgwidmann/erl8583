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
%% @doc This module marshalls a iso8583message() field into 
%%      an ASCII string.

-module(erl8583_marshaller_ascii_field).

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

%% @doc Marshals a data element into a string given the field encoding
%%      and the value of the data element.
%%
%% @spec marshal_data_element(Encoding::field_encoding(), iso8583field_value()) -> string()
-spec(marshal_data_element(field_encoding(), iso8583field_value()) -> string()).

marshal_data_element({n, llvar, Length}, Value) when length(Value) =< Length ->
	erl8583_convert:integer_to_string(length(Value), 2) ++ Value;
marshal_data_element({n, lllvar, Length}, Value) when length(Value) =< Length ->
	erl8583_convert:integer_to_string(length(Value), 3) ++ Value;
marshal_data_element({ns, llvar, Length}, Value) when length(Value) =< Length ->
	erl8583_convert:integer_to_string(length(Value), 2) ++ Value;
marshal_data_element({an, llvar, Length}, Value) when length(Value) =< Length ->
	erl8583_convert:integer_to_string(length(Value), 2) ++ Value;
marshal_data_element({an, lllvar, Length}, Value) when length(Value) =< Length ->
	erl8583_convert:integer_to_string(length(Value), 3) ++ Value;
marshal_data_element({ans, llvar, Length}, Value) when length(Value) =< Length ->
	erl8583_convert:integer_to_string(length(Value), 2) ++ Value;
marshal_data_element({ans, lllvar, Length}, Value) when length(Value) =< Length ->
	erl8583_convert:integer_to_string(length(Value), 3) ++ Value;
marshal_data_element({n, fixed, Length}, Value) when length(Value) =< Length ->
	IntValue = list_to_integer(Value),
	erl8583_convert:integer_to_string(IntValue, Length);
marshal_data_element({an, fixed, Length}, Value) when length(Value) =< Length ->
	erl8583_convert:pad_with_trailing_spaces(Value, Length);
marshal_data_element({ans, fixed, Length}, Value) when length(Value) =< Length ->
	erl8583_convert:pad_with_trailing_spaces(Value, Length);
marshal_data_element({x_n, fixed, Length}, [Head | Value]) when Head =:= $C orelse Head =:= $D ->
	IntValue = list_to_integer(Value),
	[Head] ++ erl8583_convert:integer_to_string(IntValue, Length);
marshal_data_element({z, llvar, Length}, Value) when length(Value) =< Length ->
	erl8583_convert:integer_to_string(length(Value), 2) ++ Value;
marshal_data_element({b, fixed, Length}, Value) when size(Value) =:= Length ->
	erl8583_convert:binary_to_ascii_hex(Value).

%% @doc Extracts a field value from the start of a string given how the field
%%      is encoded.  The field value and the rest of the unmarshalled string
%%      is returned as a 2-tuple.
%%
%% @spec unmarshal_data_element(Encoding::field_encoding(), string()) -> {iso8583field_value(), string()}
-spec(unmarshal_data_element(field_encoding(), string()) -> {iso8583field_value(), string()}).

unmarshal_data_element({n, llvar, _MaxLength}, Fields) ->
	{N, Rest} = lists:split(2, Fields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({n, lllvar, _MaxLength}, Fields) ->
	{N, Rest} = lists:split(3, Fields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({ns, llvar, _MaxLength}, Fields) ->
	{N, Rest} = lists:split(2, Fields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({an, llvar, _MaxLength}, Fields) ->
	{N, Rest} = lists:split(2, Fields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({an, lllvar, _MaxLength}, Fields) ->
	{N, Rest} = lists:split(3, Fields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({ans, llvar, _MaxLength}, Fields) ->
	{N, Rest} = lists:split(2, Fields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({ans, lllvar, _MaxLength}, Fields) ->
	{N, Rest} = lists:split(3, Fields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({n, fixed, Length}, Fields) ->
	lists:split(Length, Fields);
unmarshal_data_element({an, fixed, Length}, Fields) ->
	lists:split(Length, Fields);
unmarshal_data_element({ans, fixed, Length}, Fields) ->
	lists:split(Length, Fields);
unmarshal_data_element({x_n, fixed, Length}, [Head|Tail]) when Head =:= $C orelse Head =:= $D ->
	lists:split(Length+1, [Head|Tail]);
unmarshal_data_element({z, llvar, _MaxLength}, Fields) ->
	{N, Rest} = lists:split(2, Fields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({b, fixed, Length}, Fields) ->
	{ValueStr, Rest} = lists:split(2 * Length, Fields),
	Value = erl8583_convert:ascii_hex_to_binary(ValueStr),
	{Value, Rest}.

%% @doc Marshals a field value into an ASCII string.
%%
%% @spec marshal(integer(), iso8583field_value()) -> string()
-spec(marshal(integer(), iso8583field_value()) -> string()).

marshal(FieldId, Value) ->
	Pattern = erl8583_fields:get_encoding(FieldId),
	marshal_data_element(Pattern, Value).

%% @doc Extracts a field value from the start of a string.  The field value 
%%      and the rest of the unmarshalled string is returned as a 2-tuple.
%%
%% @spec unmarshal(integer(), string()) -> {iso8583field_value(), string()}
-spec(unmarshal(integer(), string()) -> {iso8583field_value(), string()}).

unmarshal(FieldId, Fields) ->
	Pattern = erl8583_fields:get_encoding(FieldId),
	unmarshal_data_element(Pattern, Fields).


%%
%% Local Functions
%%

