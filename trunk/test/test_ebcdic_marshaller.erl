%% Author: carl
%% Created: 27 Mar 2011
%% Description: TODO: Add description to test_ebcdic_marshaller
-module(test_ebcdic_marshaller).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("erl8583_field_ids.hrl").

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%



%%
%% Local Functions
%%

%% Test that a message with only an MTI can be exported.
mti_only_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	<<240, 242, 240, 240>> = erl8583_marshaller_ebcdic:marshal(Msg2),
	Msg3 = erl8583_message:set(0, "0210", Msg1),
	<<240, 242, 241, 240>> = erl8583_marshaller_ebcdic:marshal(Msg3).

