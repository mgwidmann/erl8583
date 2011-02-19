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
-export([new/0, 
		 new/1, 
		 set/3, 
		 get/2, 
		 get_fields/1, 
		 to_list/1, 
		 from_list/1, 
		 set_attributes/2, 
		 get_attributes/1]).

%%
%% API Functions
%%
new() ->
	new([]).

new(Attributes) ->
	{iso8583_message, Attributes, dict:new()}.
	
set(Index, Value, Msg) when is_integer(Index) andalso Index >= 0 ->
	{iso8583_message, Attrs, Dict} = Msg,
	false = dict:is_key(Index, Dict),
	{iso8583_message, Attrs, dict:store(Index, Value, Dict)}.
	
get(Index, Msg) ->
	{iso8583_message, _Attrs, Dict} = Msg,
	dict:fetch(Index, Dict).

get_fields(Msg) ->
	{iso8583_message, _Attrs, Dict} = Msg,
	lists:sort(dict:fetch_keys(Dict)).

to_list(Msg) ->
	{iso8583_message, _Attrs, Dict} = Msg,
	dict:to_list(Dict).

get_attributes(Msg) ->
	{iso8583_message, Attrs, _} = Msg,
	Attrs.
									
from_list(List) ->
	from_list(List, new()).

set_attributes(Attr, Msg) ->
	{iso8583_message, [], Dict} = Msg,
	{iso8583_message, Attr, Dict}.

%%
%% Local Functions
%%
from_list([], Result) ->
	Result;
from_list([{Key, Value}|Tail], Result) ->
	from_list(Tail, set(Key, Value, Result)).
