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
		 repeat/1
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




%%
%% Local Functions
%%

