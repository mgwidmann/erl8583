%% Author: carl
%% Created: 12 Nov 2011
%% Description: TODO: Add description to test_json_marshaller
-module(test_json_marshaller).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("erl8583_marshallers.hrl").

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%
marshal_init_test() ->
	Msg = erl8583_message:new(),
	{[], Msg} = erl8583_marshaller_json:marshal_init(Msg).



%%
%% Local Functions
%%

