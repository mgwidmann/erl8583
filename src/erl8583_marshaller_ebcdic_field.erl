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
%%      a binary containing EBCDIC characters or unmarshals a binary
%%      containing EBCDIC characters into an iso8583message() field.

-module(erl8583_marshaller_ebcdic_field).

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

%% @doc Marshals a data element into an EBCDIC binary given the field encoding
%%      and the value of the data element.
%%
%% @spec marshal_data_element(Encoding::field_encoding(), iso8583field_value()) -> binary()
-spec(marshal_data_element(field_encoding(), iso8583field_value()) -> binary()).

marshal_data_element(Encoding, FieldValue) ->
	AsciiEnc = erl8583_marshaller_ascii_field:marshal_data_element(Encoding, FieldValue),
	erl8583_convert:ascii_to_ebcdic(AsciiEnc).

%% @doc Extracts a field value from the start of an EBCDIC binary given how the field
%%      is encoded.  The field value and the rest of the unmarshalled EBCDIC binary
%%      is returned as a 2-tuple.
%%
%% @spec unmarshal_data_element(Encoding::field_encoding(), binary()) -> {iso8583field_value(), binary()}
-spec(unmarshal_data_element(field_encoding(), binary()) -> {iso8583field_value(), binary()}).

unmarshal_data_element(_,_) ->
	{<<>>, <<>>}.

%% @doc Marshals a field value into an EBCDIC binary. The 1987 version
%%      of the ISO 8583 specification is used to determine how to
%%      encode the field value.
%%
%% @spec marshal_field(integer(), iso8583field_value()) -> binary()
-spec(marshal_field(integer(), iso8583field_value()) -> binary()).

marshal_field(FieldId, FieldValue) ->
	Pattern = erl8583_fields:get_encoding(FieldId),
	marshal_data_element(Pattern, FieldValue).

%% @doc Extracts a field value from the start of an EBCDIC binary.  The field value 
%%      and the rest of the unmarshalled EBCDIC binary are returned as a 2-tuple.
%%      The 1987 version of the ISO 8583 specification is used to determine how to
%%      decode the field value.
%%
%% @spec unmarshal_field(integer(), binary()) -> {iso8583field_value(), binary()}
-spec(unmarshal_field(integer(), binary()) -> {iso8583field_value(), binary()}).

unmarshal_field(_, _) ->
	{<<>>, <<>>}.


%%
%% Local Functions
%%

