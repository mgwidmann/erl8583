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
	Marshalled2 = binary_marshaller:marshall(Msg2),
	{<<2, 0>>, X} = split_binary(Marshalled2, 2),
	Msg3 = iso8583_message:set(0, "0210", Msg1),
	Marshalled3 = binary_marshaller:marshall(Msg3),
	{<<2, 16>>, X} = split_binary(Marshalled3, 2).



%%
%% Local Functions
%%

