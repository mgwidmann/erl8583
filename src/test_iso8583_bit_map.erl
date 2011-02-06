%% Author: carl
%% Created: 05 Feb 2011
%% Description: TODO: Add description to test_iso8583_bit_field
-module(test_iso8583_bit_map).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%
new_test() ->
	{iso8583_bit_map, _} = iso8583_bit_map:new().
								
set_test() ->
	BitMap = iso8583_bit_map:new(),
	{iso8583_bit_map, _} = iso8583_bit_map:set(0, "0200", BitMap),
	{iso8583_bit_map, _} = iso8583_bit_map:set(65, "hello", BitMap).

set_field_negative_test() ->
	BitMap = iso8583_bit_map:new(),
	?assertError(_, iso8583_bit_map:set(-1, "0200", BitMap)).
	
set_field_index_twice_test() ->
	BitMap = iso8583_bit_map:new(),
	UpdatedBitMap = iso8583_bit_map:set(64, "hello", BitMap),
	?assertError(_, iso8583_bit_map:set(64, "hello", UpdatedBitMap)).
	
get_test() ->
	BitMap = iso8583_bit_map:new(),
	UpdatedBitMap = iso8583_bit_map:set(64, "hello", BitMap),
	"hello" = iso8583_bit_map:get(64, UpdatedBitMap).

get_no_value_test() ->
	BitMap = iso8583_bit_map:new(),
	?assertError(_, iso8583_bit_map:get(64, BitMap)).

get_indexes_empty_test() ->
	BitMap = iso8583_bit_map:new(),
	[] = iso8583_bit_map:get_indexes(BitMap).

get_indexes_test() ->
	BitMap1 = iso8583_bit_map:new(),
	BitMap2 = iso8583_bit_map:set(64, "hello", BitMap1),
	BitMap3 = iso8583_bit_map:set(0, 7, BitMap2),
	BitMap4 = iso8583_bit_map:set(65, "bar", BitMap3),
	[0, 64, 65] = iso8583_bit_map:get_indexes(BitMap4).

to_list_test() ->
	BitMap1 = iso8583_bit_map:new(),
	BitMap2 = iso8583_bit_map:set(64, "hello", BitMap1),
	BitMap3 = iso8583_bit_map:set(0, "foo", BitMap2),
	BitMap4 = iso8583_bit_map:set(65, 7, BitMap3),
	%[{0, "foo"}, {64, "hello"}, {65, 7}] = 
	iso8583_bit_map:to_list(BitMap4).

from_list_test() ->
	BitMap = iso8583_bit_map:from_list([{0, "0200"}, {39, 0}]),
	{iso8583_bit_map, _} = BitMap,
	[{0, "0200"}, {39, 0}] = iso8583_bit_map:to_list(BitMap).

%%
%% Local Functions
%%

