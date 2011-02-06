%% Author: carl
%% Created: 05 Feb 2011
%% Description: TODO: Add description to iso8583_bit_field
-module(iso8583_bit_map).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([new/0, set/3, get/2, get_indexes/1, to_list/1, from_list/1]).

%%
%% API Functions
%%
new() ->
	{iso8583_bit_map, dict:new()}.

set(Index, Value, BitMap) when Index >= 0 ->
	{iso8583_bit_map, Dict} = BitMap,
	case dict:is_key(Index, Dict) of
		false ->
			{iso8583_bit_map, dict:store(Index, Value, Dict)}
	end.

get(Index, BitMap) when is_integer(Index) ->
	{iso8583_bit_map, Dict} = BitMap,
	dict:fetch(Index, Dict).

get_indexes(BitMap) ->
	{iso8583_bit_map, Dict} = BitMap,
	KeyValues = dict:to_list(Dict),
	Keys = [K || {K, _V} <- KeyValues],
	lists:sort(Keys).

to_list(BitMap) ->
	{iso8583_bit_map, Dict} = BitMap,
	Keys = get_indexes(BitMap),
	[{K, dict:fetch(K, Dict)} || K <- Keys].

from_list(List) ->
	from_list(List, new()).

%%
%% Local Functions
%%
from_list([], Result) ->
	Result;
from_list([{Key, Value}|Tail], Result) ->
	from_list(Tail, set(Key, Value, Result)).
