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
-export([new/1, set/3]).

%%
%% API Functions
%%
new(NumFields) ->
	Dict = dict:new(),
	UpdatedDict = dict:store(max_index, NumFields, Dict),
	{iso8583_bit_map, UpdatedDict}.

set(Index, _Value, BitMap) when Index >= 0 ->
	{iso8583_bit_map, Dict} = BitMap,
	MaxIndex = dict:fetch(max_index, Dict),
	case Index =< MaxIndex of
		true ->
			{iso8583_bit_map, Dict}
	end.

%%
%% Local Functions
%%

