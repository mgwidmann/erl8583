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
-export([marshal_field/3, marshal_bitmap/1]).

%%
%% API Functions
%%
marshal_field(0, "0200", erl8583_fields) ->
	[0,2,0,0].

marshal_bitmap([1, 2, 3]) ->
	"bitmap = 123".

mti_test() ->
	Message = erl8583_message:set(0, "0200", erl8583_message:new()),
	[0, 2, 0, 0] = erl8583_marshaller:marshal(Message, [{field_marshaller, ?MODULE}]).

bitmap_test() ->
	Message0 = erl8583_message:set(0, "0200", erl8583_message:new()),
	Message1 = erl8583_message:set(1, "1", Message0),
	Message2 = erl8583_message:set(2, "1", Message1),	
	Message3 = erl8583_message:set(3, "1", Message2),
	"bitmap = 123" = erl8583_marshaller:marshal(Message3, [{bitmap_marshaller, ?MODULE}]).

	
%%
%% Local Functions
%%

