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
-export([marshal/1 %, marshal/2, unmarshal/1, unmarshal/2, construct_bitmap/1, extract_fields/1]).
]).

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
	Mti = erl8583_convert:ascii_to_ebcdic(erl8583_message:get(0, Message)),
	[0|Fields] = erl8583_message:get_fields(Message),
	MtiBitMap = erl8583_convert:concat_binaries(Mti, construct_bitmap(Message)),
	erl8583_convert:concat_binaries(MtiBitMap, encode(Fields, Message, FieldMarshaller)).
	
%% @doc Constructs an EBCDIC binary representation of the
%%      bitmap for a list of field IDs.
%%
%% @spec construct_bitmap(list(integer())) -> binary()
-spec(construct_bitmap(list(integer())) -> binary()).

construct_bitmap(Message) ->
	AsciiBitMap = erl8583_marshaller_ascii_bitmap:marshal(Message),
	erl8583_convert:ascii_to_ebcdic(AsciiBitMap).
	%list_to_binary(AsciiBitMap).



%%
%% Local Functions
%%
encode(Fields, Msg, FieldMarshaller) ->
	encode(Fields, Msg, <<>>, FieldMarshaller).

encode([], _Msg, Result, _FieldMarshaller) ->
	Result;
encode([FieldId|Tail], Msg, Result, FieldMarshaller) ->
	Value = erl8583_message:get(FieldId, Msg),
	EncodedValue = FieldMarshaller:marshal(FieldId, Value),
	encode(Tail, Msg, erl8583_convert:concat_binaries(Result, EncodedValue), FieldMarshaller).

