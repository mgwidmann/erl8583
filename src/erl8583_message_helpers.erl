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
%% @doc Functions for manipulating erl8583_messages such as changing the
%%      message type to indicate that a message is a repeat. Functions
%%      exist to construct a new message from an old one, retaining a 
%%      subset of the message fields which can be useful when constructing
%%      a response message.
-module(erl8583_message_helpers).

%%
%% Include files
%%
%% @headerfile "../include/erl8583_types.hrl"
-include("erl8583_types.hrl").
-include("erl8583_field_ids.hrl").

%%
%% Exported Functions
%%
-export([
		 repeat/1,
		 response/1,
		 response/2
		]).

%%
%% API Functions
%%
%% @doc Updates the message type of a message to indicate that it's a repeat.
%%
%% @spec repeat(iso8583message()) -> iso8583message()
-spec(repeat(iso8583message()) -> iso8583message()).

repeat(Message) ->
	[M1, M2, M3, M4] = erl8583_message:get(?MTI, Message),
	if 
		M4 =:= $0 orelse M4 =:= $2 orelse M4 =:= $4 ->
			M4Updated = M4 + 1;
		M4 =:= $1 orelse M4 =:= $3 orelse M4 =:= $5 ->
			M4Updated = M4
	end,
	erl8583_message:set(?MTI, [M1, M2, M3, M4Updated], Message).

%% @doc Creates a response message for a message where the response has
%%      the same field values as the original message. The MTI is changed 
%%      to indicate that the message is a response.
%%
%% @spec response(iso8583message()) -> iso8583message()
-spec(response(iso8583message()) -> iso8583message()).

response(Message) ->
	response(erl8583_message:get_fields(Message), Message).

%% @doc Creates a response message for a message where the response has
%%      the same field values as the original message for a list of
%%      specified field IDs. The MTI is changed to indicate that
%%      the message is a response.
%%
%% @spec response(list(integer()), iso8583message()) -> iso8583message()
-spec(response(list(integer()), iso8583message()) -> iso8583message()).

response(FieldIds, Message) ->
	Clone = clone_fields(FieldIds, Message),
	[M1, M2, M3, M4] = erl8583_message:get(?MTI, Message),
	if
		M3 =:= $0 orelse M3 =:= $2 orelse M3 =:= $4 ->
			% Ignore repeats.
			erl8583_message:set(?MTI, [M1, M2, M3 + 1, (M4 div 2) * 2], Clone)
	end.




%%
%% Local Functions
%%
clone_fields(FieldIds, Message) ->
	Clone = clone_fields(FieldIds, Message, erl8583_message:new()),
	erl8583_message:set_attributes(erl8583_message:get_attributes(Message), Clone).

clone_fields([], _Msg, Result) ->
	Result;
clone_fields([FieldId|Tail], Msg, Result) ->
	clone_fields(Tail, Msg, erl8583_message:set(FieldId, erl8583_message:get(FieldId, Msg), Result)).
	
