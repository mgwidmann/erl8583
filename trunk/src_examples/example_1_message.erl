%% An example that demonstrates setting and getting data elements in an
%% iso8583message().
-module(example_1_message).

%%
%% Include files
%%
-include_lib("erl8583/include/erl8583_field_ids.hrl").

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
	
	% Set the MTI (field 0), card acceptor name/location (field 43) and a MAC (field 64).
	Msg2 = erl8583_message:set_mti("0200", Msg1),
	Msg3 = erl8583_message:set(?CARD_ACCEPTOR_NAME_LOCATION, "ZIB Head Office ATM    V/I Lagos    01NG", Msg2),
	Msg4 = erl8583_message:set(?MESSAGE_AUTHENTICATION_CODE, <<1,2,3,4,5,6,7,8>>, Msg3),
	
	% Display the fields defined for the message.
	io:format("Fields: ~w~n", [erl8583_message:get_fields(Msg4)]),
	
	% Display fields 0, 43 and 64.
	io:format("MTI:                         ~s~n", [erl8583_message:get(0, Msg4)]),
	io:format("Card acceptor name/location: ~s~n", [erl8583_message:get(43, Msg4)]),
	io:format("MAC:                         ~p~n", [erl8583_message:get(64, Msg4)]).
