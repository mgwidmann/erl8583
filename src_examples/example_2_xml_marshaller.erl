%% Author: carl
%% Created: 26 Apr 2011
%% Description: TODO: Add description to example_2_xml_marshaller
-module(example_2_xml_marshaller).

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
	
	io:format("Marshalled:~n~p~n", [erl8583_marshaller_xml:marshal(Msg4)]).
