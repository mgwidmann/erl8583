%% Author: carl
%% Created: 13 Feb 2011
%% Description: TODO: Add description to test_ascii_unmarshaller
-module(test_ascii_unmarshaller).

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



%%
%% Local Functions
%%
unmarshall_mti_test() ->
	Msg = ascii_unmarshaller:unmarshall("0210"),
	"0210" = iso8583_message:get(0, Msg),
	[0] = iso8583_message:get_fields(Msg).

