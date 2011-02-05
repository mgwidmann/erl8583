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
	{iso8583_bit_map, _} = iso8583_bit_map:new(10).
								
set_test() ->
	BitMap = iso8583_bit_map:new(65),
	{iso8583_bit_map, _} = iso8583_bit_map:set(0, "0200", BitMap),
	{iso8583_bit_map, _} = iso8583_bit_map:set(65, "hello", BitMap).

set_field_negative_test() ->
	BitMap = iso8583_bit_map:new(64),
	?assertError(_, iso8583_bit_map:set(-1, "0200", BitMap)).
	
set_field_index_too_big_test() ->
	BitMap = iso8583_bit_map:new(64),
	?assertError(_, iso8583_bit_map:set(65, "hello", BitMap)).
	
%%
%% Local Functions
%%

