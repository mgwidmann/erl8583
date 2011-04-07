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
-export([marshal_field/3, unmarshal_field/3]).
-export([marshal_mti/1, unmarshal_mti/1]).
-export([marshal_bitmap/1, unmarshal_bitmap/1]).

%%
%% API Functions
%%

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

unmarshal_bitmap(Marshalled) ->
	Length = get_bitmap_length(Marshalled),
	{BitmapEbcdic, Rest} = lists:split(Length, Marshalled),
	{Bitmap, []} = erl8583_marshaller_ascii:unmarshal_bitmap(erl8583_convert:ebcdic_to_ascii(BitmapEbcdic)),
	{Bitmap, Rest}.

%% Local Functions
%%
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

get_bitmap_length(Msg) ->
	get_bitmap_length(Msg, 16).

get_bitmap_length(Msg, Length) ->
	[HexDig1, HexDig2|_Tail] = Msg,
	EbcdicDigits = [HexDig1, HexDig2],
	<<Byte>> = erl8583_convert:ascii_hex_to_binary(erl8583_convert:ebcdic_to_ascii(EbcdicDigits)),
	case (Byte band 128) of
		0 ->
			Length;
		_ ->
			{_Msg1, Msg2} = lists:split(16, Msg),
			get_bitmap_length(Msg2, Length+16)
	end.
