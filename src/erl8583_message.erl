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
%% @doc erl8583_message. Provides methods for creating, updating and
%%      interrogating an ISO 8583 message. 
%% @end

-module(erl8583_message).

%%
%% Include files
%%
%% @headerfile "../include/erl8583_types.hrl"
-include("erl8583_types.hrl").

%%
%% Exported Functions
%%
-export([new/0, 
		 new/1, 
		 set/3, 
		 get/2, 
		 get_fields/1, 
		 to_list/1, 
		 from_list/1, 
		 set_attributes/2, 
		 get_attributes/1,
		 update/3]).

%%
%% API Functions
%%

%% @doc Returns an empty ISO 8583 message.
%%
%% @spec new() -> iso8583message()
-spec(new() -> iso8583message()).

new() ->
	new([]).

%% @doc Returns an empty ISO 8583 message with a set of attributes.
%%
%% @spec new(list(iso8583attribute())) -> iso8583message()
-spec(new(list(iso8583attribute())) -> iso8583message()).

new(Attributes) ->
	{iso8583_message, Attributes, dict:new()}.
	
%% @doc Sets the value of a field in a message and returns an updated
%%      message. If the value for the field is already set, an exception
%%      is thrown.
%%
%% @spec set(integer(), iso8583field_value(), iso8583message()) -> iso8583message()
-spec(set(integer(), iso8583field_value(), iso8583message()) -> iso8583message()).

set(Index, Value, Msg) when is_integer(Index) andalso Index >= 0 ->
	{iso8583_message, Attrs, Dict} = Msg,
	false = dict:is_key(Index, Dict),
	{iso8583_message, Attrs, dict:store(Index, Value, Dict)}.
	
%% @doc Gets the value of a field from a message.
%%
%% @spec get(integer(), iso8583message()) -> iso8583field_value()
-spec(get(integer(), iso8583message()) -> iso8583field_value()).

get(Index, Msg) ->
	{iso8583_message, _Attrs, Dict} = Msg,
	dict:fetch(Index, Dict).

%% @doc Gets the fields from a message.
%%
%% @spec get_fields(iso8583message()) -> list(integer())
-spec(get_fields(iso8583message()) -> list(integer())).

get_fields(Msg) ->
	{iso8583_message, _Attrs, Dict} = Msg,
	lists:sort(dict:fetch_keys(Dict)).

%% @doc Returns an encoding of a message as a list of
%%      {Field, Value} pairs.
%%
%% @spec to_list(iso8583message()) -> list({integer(), iso8583field_value()})
-spec(to_list(iso8583message()) -> list({integer(), iso8583field_value()})).

to_list(Msg) ->
	{iso8583_message, _Attrs, Dict} = Msg,
	dict:to_list(Dict).

%% @doc Returns a list of attributes of a 
%%      message.
%%
%% @spec get_attributes(iso8583message()) -> list(iso8583attribute())
-spec(get_attributes(iso8583message()) -> list(iso8583attribute())).

get_attributes(Msg) ->
	{iso8583_message, Attrs, _Dict} = Msg,
	Attrs.
									
%% @doc Constructs an ISO 8583 message from a list
%%      of {Id, Value} pairs.
%%
%% @spec from_list({integer(), iso8583field_value()})-> iso8583message()
-spec(from_list({integer(), iso8583field_value()})-> iso8583message()).

from_list(List) ->
	from_list(List, new()).

%% @doc Sets the attributes for a message.
%%
%% @spec set_attributes(list(iso8583attribute()), iso8583message())-> iso8583message()
-spec(set_attributes(list(iso8583attribute()), iso8583message())-> iso8583message()).

set_attributes(Attr, Msg) ->
	{iso8583_message, [], Dict} = Msg,
	{iso8583_message, Attr, Dict}.

%% @doc Sets or updates the value of a field in a message and returns an updated
%%      message. The value for the field need not have been set previously.
%%
%% @spec update(integer(), iso8583field_value(), iso8583message()) -> iso8583message()
-spec(update(integer(), iso8583field_value(), iso8583message()) -> iso8583message()).

update(Index, Value, Msg) when is_integer(Index) andalso Index >= 0 ->
	{iso8583_message, Attrs, Dict} = Msg,
	{iso8583_message, Attrs, dict:store(Index, Value, Dict)}.

%%
%% Local Functions
%%
from_list([], Result) ->
	Result;
from_list([{Key, Value}|Tail], Result) ->
	from_list(Tail, set(Key, Value, Result)).
