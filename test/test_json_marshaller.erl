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

marshal_string_test() ->
	MarshalledField = erl8583_marshaller_json:marshal_field(2, "0123456789", ?MODULE),
	"\"2\" : \"0123456789\", " = MarshalledField.
	
marshal_binary_test() ->
	MarshalledField = erl8583_marshaller_json:marshal_field(64, <<0,1,16,255>>, ?MODULE),
	"\"64\" : \"000110FF\", " = MarshalledField.
	
%%
%% Local Functions
%%

