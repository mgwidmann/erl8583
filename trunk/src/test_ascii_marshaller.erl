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
	"30323030" ++ X = ascii_marshaller:marshall(Msg2),
	Msg3 = iso8583_message:set(0, "0210", Msg1),
	"30323130" ++ X = ascii_marshaller:marshall(Msg3).

%%
%% Local Functions
%%

