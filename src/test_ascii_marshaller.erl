%% Author: carl
%% Created: 12 Feb 2011
%% Description: TODO: Add description to test_ascii_marshaller
-module(test_ascii_marshaller).

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

%% Test that a message with only an MTI can be exported.
mti_only_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	"0200" ++ X = ascii_marshaller:marshall(Msg2),
	Msg3 = iso8583_message:set(0, "0210", Msg1),
	"0210" ++ X = ascii_marshaller:marshall(Msg3).

pan_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(2, "5234567890123456", Msg2),	
	"02004000000000000000165234567890123456" = ascii_marshaller:marshall(Msg3).

proc_code_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0100", Msg1),
	Msg3 = iso8583_message:set(3, "01234", Msg2),	
	"01002000000000000000001234" = ascii_marshaller:marshall(Msg3).

amount_tran_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(4, "30000", Msg2),	
	"02001000000000000000000000030000" = ascii_marshaller:marshall(Msg3).
	
amount_settle_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(5, "1", Msg2),	
	"02000800000000000000000000000001" = ascii_marshaller:marshall(Msg3).

fields_5_6_7_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(5, "1", Msg2),	
	Msg4 = iso8583_message:set(6, "2", Msg3),	
	Msg5 = iso8583_message:set(7, "0131081200", Msg4),	
	"02000E000000000000000000000000010000000000020131081200" = ascii_marshaller:marshall(Msg5).

fields_8_9_10_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "1300", Msg1),
	Msg3 = iso8583_message:set(8, "1", Msg2),	
	Msg4 = iso8583_message:set(9, "2", Msg3),	
	Msg5 = iso8583_message:set(10, "3", Msg4),	
	"130001C0000000000000000000010000000200000003" = ascii_marshaller:marshall(Msg5).
	
%%
%% Local Functions
%%

