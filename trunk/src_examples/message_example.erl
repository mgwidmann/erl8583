%% Author: carl
%% Created: 26 Apr 2011
%% Description: TODO: Add description to message_example
-module(message_example).

%%
%% Include files
%%
-include("erl8583_field_ids.hrl").

%%
%% Exported Functions
%%
-export([test/0]).

%%
%% API Functions
%%
test() ->
	% Create a message.
	Msg1 = erl8583_message:new(),
	
	% Set the MTI (field 0) and a MAC (field 64).
	Msg2 = erl8583_message:set_mti("0200", Msg1),
	Msg3 = erl8583_message:set(?MESSAGE_AUTHENTICATION_CODE, <<1,2,3,4,5,6,7,8>>, Msg2),
	
	% Display the MTI and MAC.
	io:format("MTI: ~p~n", [erl8583_message:get(0, Msg3)]),
	io:format("MAC: ~p~n", [erl8583_message:get(64, Msg3)]).
