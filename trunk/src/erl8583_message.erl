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
		 set/3, 
		 set_mti/2,
		 get/2, 
		 get_mti/1,
		 get_fields/1,
		 remove/2,
		 is_message/1,
		 get_attributes/1,
		 get_attribute/2,
		 set_attributes/2, 
		 set_attribute/3
		]).

%%
%% API Functions
%%

%% @doc Returns an empty ISO 8583 message.
%%
%% @spec new() -> iso8583message()
-spec(new() -> iso8583message()).

new() ->
	#iso8583_message{attributes=[]}.

%% @doc Sets the value of a field in a message and returns an updated
%%      message. The field can be specified as an integer or as a
%%      list of integers.  A list of integers indicates that
%%      some field is a submessage; e.g. [127, 2] would indicate field 2
%%      in field 127 of the original message. The value must be a 
%%      string, a binary or a nested message.
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
	set(FieldId, Message3, Message);
set(FieldId, FieldValue, #iso8583_message{values=Dict}=Message) when is_integer(FieldId) andalso FieldId >= 0 ->
	ok = validate_field_value(FieldValue),
	Message#iso8583_message{values=dict:store(FieldId, FieldValue, Dict)}.

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
get(FieldId, #iso8583_message{values=Dict}) when is_integer(FieldId) ->
	dict:fetch(FieldId, Dict).

%% @doc Gets the field IDs from a message; i.e. what fields exist in
%%      a message.
%%
%% @spec get_fields(iso8583message()) -> list(integer())
-spec(get_fields(iso8583message()) -> list(integer())).

get_fields(#iso8583_message{values=Dict}) ->
	lists:sort(dict:fetch_keys(Dict)).

%% @doc Returns a list of attributes of a message.
%%
%% @spec get_attributes(iso8583message()) -> list(iso8583attribute())
-spec(get_attributes(iso8583message()) -> list(iso8583attribute())).

get_attributes(#iso8583_message{attributes=Attrs}) ->
	Attrs.

%% @doc Gets the value of an attribute of a message.
%%
%% @spec get_attribute(string(), iso8583message()) -> string()
-spec(get_attribute(string(), iso8583message()) -> string()).

get_attribute(Key, #iso8583_message{attributes=Attrs}) ->
	[Result] = [Value || {KeyId, Value} <- Attrs, KeyId =:= Key],
	Result.

%% @doc Sets the value of an attribute of a message. The attribute must
%%      not have been previously set.
%%
%% @spec set_attribute(string(), string(), iso8583message()) -> iso8583message()
-spec(set_attribute(string(), string(), iso8583message()) -> iso8583message()).

set_attribute(Key, Value, Message) ->
	UpdatedMessage = delete_attribute(Key, Message),
	Attrs = UpdatedMessage#iso8583_message.attributes,
	UpdatedMessage#iso8583_message{attributes=[{Key, Value}] ++ Attrs}.
	
%% @doc Sets the attributes for a message.
%%
%% @spec set_attributes(list(iso8583attribute()), iso8583message())-> iso8583message()
-spec(set_attributes(list(iso8583attribute()), iso8583message())-> iso8583message()).

set_attributes(Attributes, #iso8583_message{attributes=[]}=Message) ->
	Message#iso8583_message{attributes=Attributes}.

%% @doc Removes a field from a message and returns the updated message.
%%
%% @spec remove(FieldId::integer()|list(integer()), iso8583message()) -> iso8583message()
-spec(remove(FieldId::integer()|list(integer()), iso8583message()) -> iso8583message()).

remove([FieldId], Message) ->
	remove(FieldId, Message);
remove([FieldId|Tail], #iso8583_message{values=Dict}=Message) ->
	case dict:is_key(FieldId, Dict) of
		false ->
			Message;
		true ->
			UpdatedSubfield = remove(Tail, erl8583_message:get(FieldId, Message)),
			case get_fields(UpdatedSubfield) of
				[] ->
					remove(FieldId, Message);
		_ 		->
					set(FieldId, UpdatedSubfield, Message)
			end
	end;
remove(FieldId, #iso8583_message{values=Dict}=Message) ->
	UpdatedDict = dict:erase(FieldId, Dict),
	Message#iso8583_message{values=UpdatedDict}.
	

%% @doc A convenient function for setting the message type identifier (MTI)
%%      of a message.
%%
%% @spec set_mti(string(), iso8583message()) -> iso8583message()
-spec(set_mti(string(), iso8583message()) -> iso8583message()).

set_mti(Mti, Message) ->
	set(0, Mti, Message).

%% @doc A convenient function for getting the message type identifier (MTI)
%%      of a message.
%%
%% @spec get_mti(iso8583message()) -> string()
-spec(get_mti(iso8583message()) -> string()).

get_mti(Message) ->
	get(0, Message).

%% @doc A function for checking whether the type of a message is an iso8583message().
%%
%% @spec is_message(any()) -> boolean()
-spec(is_message(any()) -> boolean()).

is_message(#iso8583_message{}) ->
	true;
is_message(_NonMessage) ->
	false.
	
%%
%% Local Functions
%%
delete_attribute(Key, #iso8583_message{attributes=Attrs} = Message) ->
	UpdatedAttrs = [{KeyId, Val} || {KeyId, Val} <- Attrs, KeyId =/= Key],
	Message#iso8583_message{attributes=UpdatedAttrs}.

validate_field_value(Value) when is_binary(Value) ->
	ok;
validate_field_value([]) ->
	ok;
validate_field_value([Char|Tail]) ->
	case is_integer(Char) andalso (Char >= 0) of
		true ->
			validate_field_value(Tail);
		false ->
			throw({"Invalid erl8583_message value.", Char})
	end;
validate_field_value(Value) ->
	case is_message(Value) of
		true ->
			ok;
		false ->
			throw({"Invalid erl8583_message value.", Value})
	end.
