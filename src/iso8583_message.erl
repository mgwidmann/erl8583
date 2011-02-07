%% Author: carl
%% Created: 18 Jan 2011
%% Description: TODO: Add description to iso8583_message
-module(iso8583_message).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([new/0, new/1, set/3, get/2, get_fields/1, to_list/1, from_list/1]).

%%
%% API Functions
%%
new() ->
	new([]).

new(Options) ->
	{iso8583_message, dict:from_list(Options), dict:new()}.
	
set(Index, Value, Msg) when is_integer(Index) andalso Index >= 0 ->
	{iso8583_message, Opts, Dict} = Msg,
	case dict:is_key(Index, Dict) of
		false ->
			{iso8583_message, Opts, dict:store(Index, Value, Dict)}
	end.
	
get(Index, Msg) ->
	{iso8583_message, _Opts, Dict} = Msg,
	dict:fetch(Index, Dict).

get_fields(Msg) ->
	{iso8583_message, _Opts, Dict} = Msg,
	lists:sort(dict:fetch_keys(Dict)).

to_list(Msg) ->
	{iso8583_message, _Opts, Dict} = Msg,
	dict:to_list(Dict).

from_list(List) ->
	from_list(List, new()).

%%
%% Local Functions
%%
from_list([], Result) ->
	Result;
from_list([{Key, Value}|Tail], Result) ->
	from_list(Tail, set(Key, Value, Result)).
