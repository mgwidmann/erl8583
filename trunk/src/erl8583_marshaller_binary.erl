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
%% @doc This module marshalls an iso8583message() into 
%%      a binary.

-module(erl8583_marshaller_binary).

%%
%% Include files
%%
%% @headerfile "../include/erl8583_types.hrl"
-include("erl8583_types.hrl").
-include("erl8583_field_ids.hrl").

%%
%% Exported Functions
%%
-export([marshal/1, marshal/2, marshal/3, unmarshal/1, unmarshal/2, unmarshal/3]).

%%
%% API Functions
%%

%% @doc Marshals an ISO 8583 message into a binary. This function uses
%%      the erl8583_marshaller_binary_field module to marshal the fields.
%%
%% @spec marshal(iso8583message()) -> binary()
-spec(marshal(iso8583message()) -> binary()).

marshal(Message) ->
	marshal(Message, erl8583_marshaller_binary_field).

%% @doc Marshals an ISO 8583 message into an ASCII string. This function
%%      uses the specified field marshalling module and the 
%%      erl8583_marshaller_binary_bitmap module to marshal the bit map.
%%
%% @spec marshal(iso8583message(), module()) -> binary()
-spec(marshal(iso8583message(), module()) -> binary()).

marshal(Message, FieldMarshaller) ->
	marshal(Message, FieldMarshaller, erl8583_marshaller_binary_bitmap).

%% @doc Marshals an ISO 8583 message into an ASCII string. This function
%%      uses the specified field marshalling module and the 
%%      specified bit map marshaller.
%%
%% @spec marshal(iso8583message(), module(), module()) -> binary()
-spec(marshal(iso8583message(), module(), module()) -> binary()).

marshal(Message, FieldMarshaller, BitMapMarshaller) ->
	Mti = erl8583_message:get(0, Message),
	MtiBin = FieldMarshaller:marshal(?MTI, Mti),
	BitMap = BitMapMarshaller:marshal(Message),
	[?MTI|Fields] = erl8583_message:get_fields(Message),
	EncodedFields = encode(Fields, Message, FieldMarshaller),
	MtiBin ++ BitMap ++ EncodedFields.

%% @doc Unmarshals a binary into an ISO 8583 message. This function uses
%%      the erl8583_marshaller_binary_field module to unmarshal the fields.
%%
%% @spec unmarshal(list(byte())) -> iso8583message()
-spec(unmarshal(list(byte())) -> iso8583message()).

unmarshal(BinaryMessage) ->
	unmarshal(BinaryMessage, erl8583_marshaller_binary_field).

%% @doc Unmarshals a binary into an ISO 8583 message. This function uses
%%      the specified field marshalling module and the
%%      erl8583_marshaller_binary_bitmap module to unmarshal the
%%      bit map.
%%
%% @spec unmarshal(binary(), module()) -> iso8583message()
-spec(unmarshal(binary(), module()) -> iso8583message()).

unmarshal(BinaryMessage, FieldMarshaller) ->
	unmarshal(BinaryMessage, FieldMarshaller, erl8583_marshaller_binary_bitmap).

%% @doc Unmarshals a binary into an ISO 8583 message. This function uses
%%      the specified field marshalling module and the specified
%%      bit map unmarshaller.
%%
%% @spec unmarshal(binary(), module(), module()) -> iso8583message()
-spec(unmarshal(binary(), module(), module()) -> iso8583message()).

unmarshal(BinaryMessage, FieldMarshaller, BitMapMarshaller) ->
	IsoMsg1 = erl8583_message:new(),
	{Mti, Rest} = FieldMarshaller:unmarshal(?MTI, BinaryMessage),
	IsoMsg2 = erl8583_message:set(?MTI, Mti, IsoMsg1),
	{FieldIds, Fields} = BitMapMarshaller:unmarshal(Rest),
	decode_fields(FieldIds, Fields, IsoMsg2, FieldMarshaller).

%%
%% Local Functions
%%
encode(Fields, Msg, FieldMarshaller) ->
	encode(Fields, Msg, [], FieldMarshaller).

encode([], _Msg, Result, _FieldMarshaller) ->
	Result;
encode([FieldId|Tail], Msg, Result, FieldMarshaller) ->
	Value = erl8583_message:get(FieldId, Msg),
	EncodedValue = FieldMarshaller:marshal(FieldId, Value),
	encode(Tail, Msg, Result ++ EncodedValue, FieldMarshaller).

decode_fields([], <<>>, Result, _FieldMarshaller) ->
	Result;
decode_fields([FieldId|Tail], Fields, Result, FieldMarshaller) ->
	{Value, UpdatedFields} = FieldMarshaller:unmarshal(FieldId, binary_to_list(Fields)),
	UpdatedResult = erl8583_message:set(FieldId, Value, Result),
	decode_fields(Tail, UpdatedFields, UpdatedResult, FieldMarshaller).
