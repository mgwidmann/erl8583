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
%% @doc Functions for constructing, reading and updating ISO 8583 messages.
%%      This module models an ISO 8583 message as a list of fields. A message
%%      field is an {identifier, value} pair where the identifier is
%%      a non-negative integer. The value can be an ASCII string, a binary
%%      another message. Integer values, e.g. a PAN should be encoded as
%%      strings.
%%      
%%      The identifier 0 is associated with the MTI of a message.
%%
%%      Optionally attributes can be set for a message (e.g. indicating
%%      whether it's a sent or received message).
-module(erl8583_message).

%%
%% Include files
%%
%% @headerfile "../include/erl8583_types.hrl"
-include("erl8583_types.hrl").
-include("erl8583_field_ids.hrl").

%%
%% Exported Functions
%%
-export([new/0, 
		 new/1, 
		 set/3, 
		 set/2,
		 get/2, 
		 get_fields/1, 
		 to_list/1, 
		 from_list/1, 
		 set_attributes/2, 
		 get_attributes/1,
		 update/3,
		 update/2,
		 repeat/1,
		 clone_fields/2,
		 response/1,
		 response/2,
		 remove_fields/2]).

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
%%      is thrown. The field can be specified as an integer or as a
%%      list of integers.  A list of integers indicates that
%%      some field is a submessage; e.g. [127, 2] would indicate field 2
%%      in field 127 of the original message.
%%
%% @spec set(FieldId::integer()|list(integer()), iso8583field_value(), iso8583message()) -> iso8583message()
-spec(set(FieldId::integer()|list(integer()), iso8583field_value(), iso8583message()) -> iso8583message()).

set([FieldId], FieldValue, Message) when is_integer(FieldId) ->
	set(FieldId, FieldValue, Message);
set([FieldId|Tail], FieldValue, Message) when is_integer(FieldId) ->
	case lists:member(FieldId, get_fields(Message)) of
		true ->
			Message2 = get(FieldId, Message);
		false ->
			Message2 = new()
	end,
	Message3 = set(Tail, FieldValue, Message2),
	update(FieldId, Message3, Message);
set(FieldId, FieldValue, Message) when is_integer(FieldId) andalso FieldId >= 0 ->
	{iso8583_message, Attrs, Dict} = Message,
	false = dict:is_key(FieldId, Dict),
	{iso8583_message, Attrs, dict:store(FieldId, FieldValue, Dict)}.
	
%% @doc Sets the values of zero or more fields in a message and returns an updated
%%      message. The field IDs and field values are passed as 2-tuples in 
%%      a list.
%%
%% @spec set(list({integer(), iso8583field_value()}), iso8583message()) -> iso8583message()
-spec(set(list({integer(), iso8583field_value()}), iso8583message()) -> iso8583message()).

set([], Message) ->
	Message;
set(FieldsList, Message) ->
	[{FieldId, FieldValue}|Tail] = FieldsList,
	set(Tail, set(FieldId, FieldValue, Message)).
	
%% @doc Gets the value of a field from a message given a field ID or a list
%%      of identifiers. A list of integers indicates that
%%      some field is a submessage; e.g. [127, 2] would indicate field 2
%%      in field 127 of the original message.
%%
%% @spec get(FieldId::integer()|list(integer()), iso8583message()) -> iso8583field_value()
-spec(get(FieldId::integer()|list(integer()), iso8583message()) -> iso8583field_value()).

get([FieldId], Message) when is_integer(FieldId) ->
	get(FieldId, Message);
get([FieldId|Tail], Message)  when is_integer(FieldId) ->
	Message2 = get(FieldId, Message),
	get(Tail, Message2);
get(FieldId, Message) when is_integer(FieldId) ->
	{iso8583_message, _Attrs, Dict} = Message,
	dict:fetch(FieldId, Dict).

%% @doc Gets the field IDs from a message; i.e. what fields exist in
%%      a message.
%%
%% @spec get_fields(iso8583message()) -> list(integer())
-spec(get_fields(iso8583message()) -> list(integer())).

get_fields(Message) ->
	{iso8583_message, _Attrs, Dict} = Message,
	lists:sort(dict:fetch_keys(Dict)).

%% @doc Returns an encoding of a message as a list of
%%      {Field, Value} pairs.
%%
%% @spec to_list(iso8583message()) -> list({integer(), iso8583field_value()})
-spec(to_list(iso8583message()) -> list({integer(), iso8583field_value()})).

to_list(Message) ->
	{iso8583_message, _Attrs, Dict} = Message,
	dict:to_list(Dict).

%% @doc Returns a list of attributes of a 
%%      message.
%%
%% @spec get_attributes(iso8583message()) -> list(iso8583attribute())
-spec(get_attributes(iso8583message()) -> list(iso8583attribute())).

get_attributes(Message) ->
	{iso8583_message, Attrs, _Dict} = Message,
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

set_attributes(Attributes, Message) ->
	{iso8583_message, [], Dict} = Message,
	{iso8583_message, Attributes, Dict}.

%% @doc Sets or updates the value of a field in a message and returns an updated
%%      message. The value for the field need not have been set previously.
%%
%% @spec update(FieldId::integer()|list(integer()), iso8583field_value(), iso8583message()) -> iso8583message()
-spec(update(FieldId::integer()|list(integer()), iso8583field_value(), iso8583message()) -> iso8583message()).

update([FieldId], FieldValue, Message) when is_integer(FieldId) ->
	update(FieldId, FieldValue, Message);
update([FieldId|Tail], FieldValue, Message) when is_integer(FieldId) ->
	case lists:member(FieldId, get_fields(Message)) of
		true ->
			Message2 = get(FieldId, Message);
		false ->
			Message2 = new()
	end,
	Message3 = update(Tail, FieldValue, Message2),
	update(FieldId, Message3, Message);
update(FieldId, FieldValue, Message) when is_integer(FieldId) andalso FieldId >= 0 ->
	{iso8583_message, Attrs, Dict} = Message,
	{iso8583_message, Attrs, dict:store(FieldId, FieldValue, Dict)}.

%% @doc Sets or updates the values of zero or more fields in a message and returns an updated
%%      message. The field IDs and field values are passed as 2-tuples in 
%%      a list.
%%
%% @spec update(list({integer(), iso8583field_value()}), iso8583message()) -> iso8583message()
-spec(update(list({integer(), iso8583field_value()}), iso8583message()) -> iso8583message()).

update([], Message) ->
	Message;
update(FieldsList, Message) ->
	[{FieldId, FieldValue}|Tail] = FieldsList,
	update(Tail, update(FieldId, FieldValue, Message)).
	
%% @doc Updates the message type of a message to indicate that it's a repeat.
%%
%% @spec repeat(iso8583message()) -> iso8583message()
-spec(repeat(iso8583message()) -> iso8583message()).

repeat(Message) ->
	[M1, M2, M3, M4] = get(?MTI, Message),
	if 
		M4 =:= $0 orelse M4 =:= $2 orelse M4 =:= $4 ->
			M4Updated = M4 + 1;
		M4 =:= $1 orelse M4 =:= $3 orelse M4 =:= $5 ->
			M4Updated = M4
	end,
	update(?MTI, [M1, M2, M3, M4Updated], Message).

%% @doc Creates a new message from an old one where the new message has
%%      the same field values as the original message for a list of
%%      specified field IDs.
%%
%% @spec clone_fields(list(integer()), iso8583message()) -> iso8583message()
-spec(clone_fields(list(integer()), iso8583message()) -> iso8583message()).

clone_fields(FieldIds, Message) ->
	clone_fields(FieldIds, Message, new()).

%% @doc Creates a response message for a message where the response has
%%      the same field values as the original message. The MTI is changed 
%%      to indicate that the message is a response.
%%
%% @spec response(iso8583message()) -> iso8583message()
-spec(response(iso8583message()) -> iso8583message()).

response(Message) ->
	response(get_fields(Message), Message).

%% @doc Creates a response message for a message where the response has
%%      the same field values as the original message for a list of
%%      specified field IDs. The MTI is changed to indicate that
%%      the message is a response.
%%
%% @spec response(list(integer()), iso8583message()) -> iso8583message()
-spec(response(list(integer()), iso8583message()) -> iso8583message()).

response(FieldIds, Message) ->
	Clone = clone_fields(FieldIds, Message),
	[M1, M2, M3, M4] = get(?MTI, Message),
	if
		M3 =:= $0 orelse M3 =:= $2 orelse M3 =:= $4 ->
			update(?MTI, [M1, M2, M3 + 1, M4], Clone)
	end.

%% @doc Creates a new message from an old message where the new message
%%      has the same field values as the original except for a
%%      specified list of IDs that are omitted.
%%
%% @spec remove_fields(list(integer()), iso8583message()) -> iso8583message()
-spec(remove_fields(list(integer()), iso8583message()) -> iso8583message()).

remove_fields(FieldIds, Message) ->
	{iso8583_message, Attributes, Dict} = Message,
	UpdatedDict = remove_fields_from_dict(FieldIds, Dict),
	{iso8583_message, Attributes, UpdatedDict}.
	
%%
%% Local Functions
%%
from_list([], Result) ->
	Result;
from_list([{Key, Value}|Tail], Result) ->
	from_list(Tail, set(Key, Value, Result)).

clone_fields([], _Msg, Result) ->
	Result;
clone_fields([FieldId|Tail], Msg, Result) ->
	clone_fields(Tail, Msg, update(FieldId, get(FieldId, Msg), Result)).
	
remove_fields_from_dict([], Dict) ->
	Dict;
remove_fields_from_dict([FieldId|Tail], Dict) ->
	remove_fields_from_dict(Tail, dict:erase(FieldId, Dict)).
