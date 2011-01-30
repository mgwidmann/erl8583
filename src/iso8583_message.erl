%% Author: carl
%% Created: 18 Jan 2011
%% Description: TODO: Add description to iso8583_message
-module(iso8583_message).

%%
%% Include files
%%
-include("iso8583_defines.hrl").
-include("iso8583_field.hrl").

%%
%% Exported Functions
%%
-export([new/0, new/1, set/3, get/2, get_fields/1, to_list/1]).

%%
%% API Functions
%%
new() ->
	Dict = dict:new(),
	{iso8583_message, Dict}.

new(Options) ->
	Dict = new(Options, dict:new()),
	{iso8583_message, Dict}.	

set(Field, Value, Message) when Field >= 0 andalso Field =< ?MAX_FIELD_ID ->
		{iso8583_message, Dict} = Message,
		case dict:is_key(validator, Dict) of
			true ->
				Validator = dict:fetch(validator, Dict),
				Validator:validate_field(Field, Value);
			false ->
				ok
		end,
		case dict:is_key(Field, Dict) of
			true ->
				erlang:error(field_already_set);
			false ->
				UpdatedDict = dict:store(Field, Value, Dict),
				{iso8583_message, UpdatedDict}
		end;
set(Field, Value, Message) when is_atom(Field) ->
	Id = mapAtomToId(Field, Message),
	set(Id, Value, Message).

get(Field, Message) when is_integer(Field) ->
	{iso8583_message, Dict} = Message,
	dict:fetch(Field, Dict);
get(Field, Message) when is_atom(Field) ->
	Id = mapAtomToId(Field, Message),
	get(Id, Message).

get_fields(Message) ->
	{iso8583_message, Dict} = Message,
	KeyValues = dict:to_list(Dict),
	List = get_fields(KeyValues, []),
	lists:sort(List).

to_list(Message) ->
	{iso8583_message, Dict} = Message,
	lists:reverse(to_list(get_fields(Message), [], Dict)).

%%
%% Local Functions
%%
mapAtomToId(Atom, Message) ->
	{iso8583_message, Dict} = Message,
	Mapper = dict:fetch(mapper, Dict),
	Id = Mapper:map_atom_to_id(Atom),
	true = is_integer(Id),
	Id.	

new([], Dict) ->
	Dict;
new([{mapper, Value}|T], Dict) ->
	new(mapper, Value, T, Dict);
new([{validator, Value}|T], Dict) ->
	new(validator, Value, T, Dict).	
	
new(Key, Value, List, Dict) ->
	UpdatedDict = dict:store(Key, Value, Dict),
	new(List, UpdatedDict).

get_fields([], List) ->
	List;
get_fields([{Key, _Value}|T], List) ->
	case is_integer(Key) of
		true ->
			get_fields(T, [Key|List]);
		false ->
			get_fields(T, List)
	end.
	
to_list([], Result, _Dict) ->
	Result;
to_list([Key|T], Result, Dict) ->
	to_list(T, [{Key, dict:fetch(Key, Dict)}|Result], Dict).