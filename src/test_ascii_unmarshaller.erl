%% Author: carl
%% Created: 13 Feb 2011
%% Description: TODO: Add description to test_ascii_unmarshaller
-module(test_ascii_unmarshaller).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("field_defines.hrl").

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%



%%
%% Local Functions
%%
unmarshall_mti_test() ->
	Msg = ascii_unmarshaller:unmarshall("0210"),
	"0210" = iso8583_message:get(0, Msg),
	[0] = iso8583_message:get_fields(Msg).

pan_test() ->
	Msg = ascii_unmarshaller:unmarshall("02004000000000000000165234567890123456"),
	"0200" = iso8583_message:get(0, Msg),
	[0, 2] = iso8583_message:get_fields(Msg),
	"5234567890123456" = iso8583_message:get(2, Msg).
	
field_8_9_10_test() ->
	Msg = ascii_unmarshaller:unmarshall("130001C0000000000000000000010000000200000003"),
	"1300" = iso8583_message:get(0, Msg),
	[0, 8, 9, 10] = iso8583_message:get_fields(Msg),
	"00000001" = iso8583_message:get(8, Msg),
	"00000002" = iso8583_message:get(9, Msg),
	"00000003" = iso8583_message:get(10, Msg).

fields_11_12_13_14_test() ->
	Msg = ascii_unmarshaller:unmarshall("0200003C00000000000000123415075520121206"),
	[0, 11, 12, 13, 14] = iso8583_message:get_fields(Msg),
	"150755" = iso8583_message:get(12, Msg),
	"001234" = iso8583_message:get(11, Msg).

fields_15_to_20_test() ->
	Msg = ascii_unmarshaller:unmarshall("02000003F000000000000001000200030004005006"),
	[0, 15, 16, 17, 18, 19, 20] = iso8583_message:get_fields(Msg),
	"0001" = iso8583_message:get(15, Msg),
	"0003" = iso8583_message:get(17, Msg),
	"0004" = iso8583_message:get(18, Msg),
	"006" = iso8583_message:get(20, Msg).

field_26_test() ->
	Msg = ascii_unmarshaller:unmarshall("0200000008400000000000106"),
	[0, 21, 26] = iso8583_message:get_fields(Msg),
	"06" = iso8583_message:get(?POS_CAPTURE_CODE, Msg).
	
	
	
