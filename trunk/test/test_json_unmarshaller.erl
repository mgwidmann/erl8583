%% Author: carl
%% Created: 06 Nov 2011
%% Description: TODO: Add description to test_json_unmarshaller
-module(test_json_unmarshaller).

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
mti_unmarshall_test() ->
	Msg = "{\"fields\" : {\"0\" : \"0210\"}}",
	{"0210", Msg} = erl8583_marshaller_json:unmarshal_mti(Msg),
	Msg2 = "{\"fields\" : {\"0\" : \"0200\"}}",
	{"0200", Msg2} = erl8583_marshaller_json:unmarshal_mti(Msg2).
