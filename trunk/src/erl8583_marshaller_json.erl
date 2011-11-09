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
%% @doc This module marshals an iso8583message() 
%%      into a JSON document and can unmarshal a JSIBL document into
%%      an iso8583message(). This module also exposes
%%      functions for explicitly marshalling/unmarshalling
%%      the MTI, bitmap and fields of a message. This is
%%      to conform to the design of other marshallers and
%%      the contract required by the erl8583_marshaller.
-module(erl8583_marshaller_json).

%%
%% Include files
%%
%% @headerfile "../include/erl8583_types.hrl"
-include("erl8583_types.hrl").
-include("erl8583_marshallers.hrl").

%%
%% Exported Functions
%%
-export([marshal/1,
		 unmarshal/1,
		 unmarshal_mti/1]).

%%
%% API Functions
%%

%% @doc Constructs a JSON document of
%%      an iso8583message().
%%
%% @spec marshal(iso8583message()) -> string()
-spec(marshal(iso8583message()) -> string()).

marshal(Message) ->
	erl8583_marshaller:marshal(Message, ?MARSHALLER_JSON).

%% @doc Constructs an iso8583message() from a JSON document 
%%      representation of the message.
%%
%% @spec unmarshal(string()) -> iso8583message()
-spec(unmarshal(string()) -> iso8583message()).

unmarshal(Marshalled) ->
	erl8583_marshaller:unmarshal(Marshalled, ?MARSHALLER_JSON).

%% @doc Extracts the MTI from a JSON document.
%%      The MTI and the XML document are 
%%      returned as a 2-tuple.
%%
%% @spec unmarshal_mti(string()) -> {string(), string()}
-spec(unmarshal_mti(string()) -> {string(), string()}).

unmarshal_mti(Marshalled) ->
	{struct, JsonData} = mochijson2:decode(Marshalled),
	{struct, FieldsData} = proplists:get_value(<<"fields">>, JsonData),
	MtiBin = proplists:get_value(<<"0">>, FieldsData),
	{binary_to_list(MtiBin), Marshalled}.


%%
%% Local Functions
%%

