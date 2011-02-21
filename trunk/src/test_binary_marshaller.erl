%% Author: carl
%% Created: 19 Feb 2011
%% Description: TODO: Add description to test_binary_marshaller
-module(test_binary_marshaller).

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
%% Test that a message with only an MTI can be exported.
mti_only_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	<<2, 0>> = binary_marshaller:marshall(Msg2),
	Msg3 = iso8583_message:set(0, "0210", Msg1),
	<<2, 16>> = binary_marshaller:marshall(Msg3).

field_2_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0210", Msg1),
	Msg3 = iso8583_message:set(?PAN, "15234567890123456", Msg2),
	<<2, 16, 64, 0, 0, 0, 0, 0, 0, 0, 23, 21, 35, 69, 103, 137, 1, 35, 69, 96>> 
		= binary_marshaller:marshall(Msg3).
	
	
	



%%
%% Local Functions
%%

