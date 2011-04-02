%% Author: carl
%% Created: 02 Apr 2011
%% Description: TODO: Add description to test_marshaller
-module(test_marshaller).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([marshal_field/3]).

%%
%% API Functions
%%
marshal_field(0, "0200", erl8583_fields) ->
	[0,2,0,0].

mti_test() ->
	Message = erl8583_message:set(0, "0200", erl8583_message:new()),
	[0, 2, 0, 0] = erl8583_marshaller:marshal(Message, [{field_marshaller, ?MODULE}]).


%%
%% Local Functions
%%

