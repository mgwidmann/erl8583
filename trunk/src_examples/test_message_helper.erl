%% Author: carl
%% Created: 14 Dec 2011
%% Description: TODO: Add description to test_message_helper
-module(test_message_helper).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([]).

repeat_test() ->
	Msg = erl8583_message:new(),
	Msg1 = erl8583_message:set(0, "0301", Msg),
	Msg1Rep = erl8583_message_helper:repeat(Msg1),
	"0301" = erl8583_message:get(0, Msg1Rep),
	Msg3 = erl8583_message:set(0, "0203", Msg),
	Msg3Rep = erl8583_message_helper:repeat(Msg3),
	"0203" = erl8583_message:get(0, Msg3Rep),
	Msg5 = erl8583_message:set(0, "0205", Msg),
	Msg5Rep = erl8583_message_helper:repeat(Msg5),
	"0205" = erl8583_message:get(0, Msg5Rep),
	Msg0 = erl8583_message:set(0, "0200", Msg),
	Msg0Rep = erl8583_message_helper:repeat(Msg0),
	"0201" = erl8583_message:get(0, Msg0Rep),
	Msg2 = erl8583_message:set(0, "0402", Msg),
	Msg2Rep = erl8583_message_helper:repeat(Msg2),
	"0403" = erl8583_message:get(0, Msg2Rep),
	Msg4 = erl8583_message:set(0, "0104", Msg),
	Msg4Rep = erl8583_message_helper:repeat(Msg4),
	"0105" = erl8583_message:get(0, Msg4Rep).

