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

mti_pan_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(2, "5234567890123456", Msg2),	
	"02004000000000000000165234567890123456" = ascii_marshaller:marshall(Msg3).

%%
%% Local Functions
%%

