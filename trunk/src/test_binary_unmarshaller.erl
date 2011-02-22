%% Author: carl
%% Created: 19 Feb 2011
%% Description: TODO: Add description to test_binary_unmarshaller
-module(test_binary_unmarshaller).

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
	Msg1 = binary_unmarshaller:unmarshall(<<2, 16>>),
	"0210" = iso8583_message:get(0, Msg1),
	[0] = iso8583_message:get_fields(Msg1),
	Msg2 = binary_unmarshaller:unmarshall(<<2, 0>>),
	"0200" = iso8583_message:get(0, Msg2).
	
field_2_test() ->
	Msg1 = binary_unmarshaller:unmarshall(<<2, 16, 64, 0, 0, 0, 0, 0, 0, 0, 23, 21, 35, 69, 103, 137, 1, 35, 69, 96>>),
	"0210" = iso8583_message:get(0, Msg1),
	[0, 2] = iso8583_message:get_fields(Msg1),
	"15234567890123456" = iso8583_message:get(2, Msg1).

field_2_3_test() ->
	Msg1 = binary_unmarshaller:unmarshall(<<2, 16, 96, 0, 0, 0, 0, 0, 0, 0, 22, 18, 52, 86, 120, 144, 18, 52, 86, 1, 35, 69>>),
	"0210" = iso8583_message:get(0, Msg1),
	[0, 2, 3] = iso8583_message:get_fields(Msg1),
	"1234567890123456" = iso8583_message:get(2, Msg1),
	"012345" = iso8583_message:get(?PROC_CODE, Msg1).

field_4_test() ->
	Msg = binary_unmarshaller:unmarshall(<<18, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 35>>),
	"1200" = iso8583_message:get(0, Msg),
	[0, 4] = iso8583_message:get_fields(Msg),
	"000000000123" = iso8583_message:get(4, Msg).

field_5_test() ->
	Msg = binary_unmarshaller:unmarshall(<<18, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16>>),
	"1200" = iso8583_message:get(0, Msg),
	[0, 5] = iso8583_message:get_fields(Msg),
	"000000000010" = iso8583_message:get(5, Msg).
	
field_6_test() ->
	Msg = binary_unmarshaller:unmarshall(<<2, 32, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 135, 101>>),
	"0220" = iso8583_message:get(0, Msg),
	[0, 6] = iso8583_message:get_fields(Msg),
	"000000098765" = iso8583_message:get(6, Msg).

field_6_7_test() ->
	Msg = binary_unmarshaller:unmarshall(<<2, 32, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 152, 118, 84, 2, 34, 25, 6, 18>>),
	"0220" = iso8583_message:get(0, Msg),
	[0, 6, 7] = iso8583_message:get_fields(Msg),
	"000000987654" = iso8583_message:get(6, Msg),
	"0222190612" =  iso8583_message:get(7, Msg).
	
field_18_19_test() ->
	Msg = binary_unmarshaller:unmarshall(<<2, 32, 0, 0, 96, 0, 0, 0, 0, 0, 18, 52, 5, 103>>),
	"0220" = iso8583_message:get(0, Msg),
	[0, 18, 19] = iso8583_message:get_fields(Msg),
	"1234" = iso8583_message:get(18, Msg),
	"567" =  iso8583_message:get(19, Msg).

field_26_27_test() ->
	Msg = binary_unmarshaller:unmarshall(<<2, 33, 0, 0, 0, 96, 0, 0, 0, 0, 34, 1>>),
	"0221" = iso8583_message:get(0, Msg),
	[0, 26, 27] = iso8583_message:get_fields(Msg),
	"22" = iso8583_message:get(26, Msg),
	"1" =  iso8583_message:get(27, Msg).

fields_33_34_test() ->
	Msg = binary_unmarshaller:unmarshall(<<2, 33, 0, 0, 0, 0, 192, 0, 0, 0, 5, 18, 52, 80, 6, 53, 54, 55, 56, 57, 48>>),
	"0221" = iso8583_message:get(0, Msg),
	[0, 33, 34] = iso8583_message:get_fields(Msg),
	"12345" = iso8583_message:get(33, Msg),
	"567890" =  iso8583_message:get(34, Msg).
