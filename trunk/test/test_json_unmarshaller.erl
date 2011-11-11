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
-export([get_encoding/1]).

%%
%% API Functions
%%
get_encoding([127,2]) ->
	{n, llvar, 20}.

mti_unmarshal_test() ->
	Msg = "{\"fields\" : {\"0\" : \"0210\"}}",
	{"0210", Msg} = erl8583_marshaller_json:unmarshal_mti(Msg),
	Msg2 = "{\"fields\" : {\"0\" : \"0200\"}}",
	{"0200", Msg2} = erl8583_marshaller_json:unmarshal_mti(Msg2).

bitmap_unmarshal_1_test() ->
	Msg = "{\"fields\" : {\"1\" : \"hello\", \"127\" : \"good bye\"}}",
	{[1, 127], Msg} = erl8583_marshaller_json:unmarshal_bitmap(Msg).

bitmap_unmarshal_2_test() ->
	Msg = "{\"fields\" : {\"0\" : \"hello\", \"126\" : {\"0\" : \"good bye\"}}}",
	{[126], Msg} = erl8583_marshaller_json:unmarshal_bitmap(Msg).

field_unmarshal_test() ->
	Msg = "{\"fields\" : {\"0\" : \"0200\", \"2\" : \"12345678\"}}",
	{"12345678", Msg} = erl8583_marshaller_json:unmarshal_field(2, Msg, erl8583_fields).

binary_field_unmarshal_test() ->
	Msg = "{\"fields\" : {\"0\" : \"0200\", \"64\" : \"0011A0FF000000AA\"}}",
	{<<0,17,160,255,0,0,0,170>>, Msg} = erl8583_marshaller_json:unmarshal_field(64, Msg, erl8583_fields).
	
complex_message_test() ->
	Msg = "{\"fields\" : {\"0\" : \"0200\", \"127\" : {\"2\" : \"13579\"}}}",
	{Field127, Msg} = erl8583_marshaller_json:unmarshal_field(127, Msg, ?MODULE),
	true = erl8583_message:is_message(Field127).
	%"13579" = erl8583_message:get(2, Field127).

	