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
-export([new/0, new/1, set/3, get/2, get_fields/1, to_list/1]).

%%
%% API Functions
%%
new() ->
	new([]).

new(Options) ->
	{iso8583_message, dict:from_list(Options), iso8583_bit_map:new()}.
	
set(Id, Value, Msg) ->
	{iso8583_message, Opts, BitMap} = Msg,
	Index = mapIdToIndex(Id, Opts),
	{iso8583_message, Opts, iso8583_bit_map:set(Index, Value, BitMap)}.
	
get(Id, Msg) ->
	{iso8583_message, Opts, BitMap} = Msg,
	Index = mapIdToIndex(Id, Opts),
	iso8583_bit_map:get(Index, BitMap).

get_fields(Msg) ->
	{iso8583_message, _Opts, BitMap} = Msg,
	iso8583_bit_map:get_indexes(BitMap).

to_list(Msg) ->
	{iso8583_message, _Opts, BitMap} = Msg,
	iso8583_bit_map:to_list(BitMap).
	
%%
%% Local Functions
%%
mapIdToIndex(Id, _Opts) when is_integer(Id) ->
	Id;
mapIdToIndex(Id, Opts) ->
	Mapper = dict:fetch(mapper, Opts),
	Mapper:map_atom_to_index(Id).