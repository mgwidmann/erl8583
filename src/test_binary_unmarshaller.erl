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

										   


