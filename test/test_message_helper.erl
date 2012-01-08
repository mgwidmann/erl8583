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
	Msg1 = erl8583_message:set(0, <<"0301">>, Msg),
	Msg1Rep = erl8583_message_helpers:repeat(Msg1),
	<<"0301">> = erl8583_message:get(0, Msg1Rep),
	Msg3 = erl8583_message:set(0, <<"0203">>, Msg),
	Msg3Rep = erl8583_message_helpers:repeat(Msg3),
	<<"0203">> = erl8583_message:get(0, Msg3Rep),
	Msg5 = erl8583_message:set(0, <<"0205">>, Msg),
	Msg5Rep = erl8583_message_helpers:repeat(Msg5),
	<<"0205">> = erl8583_message:get(0, Msg5Rep),
	Msg0 = erl8583_message:set(0, <<"0200">>, Msg),
	Msg0Rep = erl8583_message_helpers:repeat(Msg0),
	<<"0201">> = erl8583_message:get(0, Msg0Rep),
	Msg2 = erl8583_message:set(0, <<"0402">>, Msg),
	Msg2Rep = erl8583_message_helpers:repeat(Msg2),
	<<"0403">> = erl8583_message:get(0, Msg2Rep),
	Msg4 = erl8583_message:set(0, <<"0104">>, Msg),
	Msg4Rep = erl8583_message_helpers:repeat(Msg4),
	<<"0105">> = erl8583_message:get(0, Msg4Rep).

response_1_test() ->
	Message = erl8583_message:new(),
	Message2 = erl8583_message:set(10, <<"hello">>, Message),
	Message3 = erl8583_message:set(0, <<"0200">>, Message2),
	Message4 = erl8583_message:set(2, <<"hello2">>, Message3),
	Response1 = erl8583_message_helpers:response([0, 10], Message4),
	[0, 10] = erl8583_message:get_fields(Response1),
	<<"hello">> = erl8583_message:get(10, Response1),
	<<"0210">> = erl8583_message:get(0, Response1),
	Response2 = erl8583_message_helpers:response([2, 10], Message4),
	[0, 2, 10] = erl8583_message:get_fields(Response2),
	<<"hello2">> = erl8583_message:get(2, Response2),
	<<"0210">> = erl8583_message:get(0, Response2).
	
response_2_test() ->
	Message = erl8583_message:new(),
	Message2 = erl8583_message:set(10, <<"hello">>, Message),
	Message3 = erl8583_message:set(0, <<"0220">>, Message2),
	Message4 = erl8583_message:set(2, <<"hello2">>, Message3),
	Response = erl8583_message_helpers:response(Message4),
	[0, 2, 10] = erl8583_message:get_fields(Response),
	<<"hello2">> = erl8583_message:get(2, Response),
	<<"0230">> = erl8583_message:get(0, Response).

% response shouldn't use the keep the repeat digit.
response_3_test() ->
	Message = erl8583_message:new(),
	Message2 = erl8583_message:set(10, <<"hello">>, Message),
	Message3 = erl8583_message:set(0, <<"0221">>, Message2),
	Response = erl8583_message_helpers:response(Message3),
	[0, 10] = erl8583_message:get_fields(Response),
	<<"hello">> = erl8583_message:get(10, Response),
	<<"0230">> = erl8583_message:get(0, Response).
	
% response shouldn't use the keep the repeat digit.
response_4_test() ->
	Message = erl8583_message:new(),
	Message2 = erl8583_message:set(0, <<"0423">>, Message),
	Response = erl8583_message_helpers:response(Message2),
	[0] = erl8583_message:get_fields(Response),
	<<"0432">> = erl8583_message:get(0, Response).
	
