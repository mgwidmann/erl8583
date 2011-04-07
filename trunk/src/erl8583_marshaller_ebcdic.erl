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
%%      an EBCDIC binary or unmarshals an EBCDIC binary into an
%%      iso8583message().
-module(erl8583_marshaller_ebcdic).

%%
%% Include files
%%
%% @headerfile "../include/erl8583_types.hrl"
-include("erl8583_types.hrl").

%%
%% Exported Functions
%%
-export([marshal/1]).
-export([marshal_field/3, unmarshal_field/3]).
-export([marshal_mti/1, unmarshal_mti/1]).
-export([marshal_bitmap/1]).

%%
%% API Functions
%%
%% @doc Marshals an ISO 8583 message into an EBCDIC binary. This function
%%      uses the erl8583_marshaller_ebcdic_field module to marshal the fields.
%%
%% @spec marshal(iso8583message()) -> binary()
-spec(marshal(iso8583message()) -> binary()).

marshal(Message) ->
	marshal(Message, erl8583_marshaller_ebcdic_field).

%% @doc Marshals an ISO 8583 message into an EBCDIC binary. This function
%%      uses the specified field marshalling module.
%%
%% @spec marshal(iso8583message(), module()) -> binary()
-spec(marshal(iso8583message(), module()) -> binary()).

marshal(Message, FieldMarshaller) ->
	Mti = erl8583_marshaller_ebcdic_field:marshal_field(0, erl8583_message:get(0, Message)),
	[0|Fields] = erl8583_message:get_fields(Message),
	Mti ++ erl8583_marshaller_ebcdic_bitmap:marshal_bitmap(Fields) ++ encode(Fields, Message, FieldMarshaller).

marshal_field(FieldId, FieldValue, EncodingRules) ->
	Ascii = erl8583_marshaller_ascii:marshal_field(FieldId, FieldValue, EncodingRules),
	erl8583_convert:ascii_to_ebcdic(Ascii).

unmarshal_field(FieldId, Marshalled, EncodingRules) ->
	Length = get_field_length(FieldId, Marshalled, EncodingRules),
	{Ebcdic, Rest} = lists:split(Length, Marshalled),
	Ascii = erl8583_convert:ebcdic_to_ascii(Ebcdic),
	{Field, []} = erl8583_marshaller_ascii:unmarshal_field(FieldId, Ascii, EncodingRules),
	{Field, Rest}.

marshal_mti(Mti) ->
	marshal_field(0, Mti, erl8583_fields).

unmarshal_mti(Marshalled) ->
	unmarshal_field(0, Marshalled, erl8583_fields).
	
marshal_bitmap(FieldIds) ->
	erl8583_convert:ascii_to_ebcdic(erl8583_marshaller_ascii:marshal_bitmap(FieldIds)).

%%
%% Local Functions
%%
encode(Fields, Msg, FieldMarshaller) ->
	encode(Fields, Msg, [], FieldMarshaller).

encode([], _Msg, Result, _FieldMarshaller) ->
	lists:reverse(Result);
encode([FieldId|Tail], Msg, Result, FieldMarshaller) ->
	Value = erl8583_message:get(FieldId, Msg),
	EncodedValue = FieldMarshaller:marshal_field(FieldId, Value),
	encode(Tail, Msg, lists:reverse(EncodedValue) ++ Result, FieldMarshaller).

get_field_length(FieldId, Marshalled, EncodingRules) ->
	Encoding = EncodingRules:get_encoding(FieldId),
	case Encoding of
		{_, fixed, N} ->
			N;
		{_, llvar, _} ->
			Nebcdic = lists:sublist(Marshalled, 1, 2),
			Nascii = erl8583_convert:ebcdic_to_ascii(Nebcdic),
			list_to_integer(Nascii) + 2;
		{_, lllvar, _} ->
			Nebcdic = lists:sublist(Marshalled, 1, 3),
			Nascii = erl8583_convert:ebcdic_to_ascii(Nebcdic),
			list_to_integer(Nascii) + 3
	end.

			