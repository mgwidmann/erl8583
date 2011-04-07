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
	[240, 242, 240, 240] = erl8583_marshaller_ebcdic:marshal(Msg2),
	Msg3 = erl8583_message:set(0, "0210", Msg1),
	[240, 242, 241, 240] = erl8583_marshaller_ebcdic:marshal(Msg3).

pan_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(2, "1234567890123456", Msg2),	
	[240, 242, 240, 240, 244, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 241, 246, 241, 242, 243, 244, 245, 246, 247, 248, 249, 240, 241, 242, 243, 244, 245, 246] = 
		erl8583_marshaller_ebcdic:marshal(Msg3).

proc_code_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0100", Msg1),
	Msg3 = erl8583_message:set(3, "01234", Msg2),	
	[240, 241, 240, 240, 242, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 241, 242, 243, 244] = 
		erl8583_marshaller_ebcdic:marshal(Msg3).

amount_tran_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(4, "30000", Msg2),	
	[240, 242, 240, 240, 241, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 243, 240, 240, 240, 240] = 
		erl8583_marshaller_ebcdic:marshal(Msg3).

amount_settle_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(5, "1", Msg2),	
	[240, 242, 240, 240, 240, 248, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 241] = 
		erl8583_marshaller_ebcdic:marshal(Msg3).

fields_5_6_7_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(5, "1", Msg2),	
	Msg4 = erl8583_message:set(6, "2", Msg3),	
	Msg5 = erl8583_message:set(7, "0131081200", Msg4),	
	[240, 242, 240, 240, 240, 197, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 241, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 242, 240, 241, 243, 241, 240, 248, 241, 242, 240, 240] = 
		erl8583_marshaller_ebcdic:marshal(Msg5).

fields_8_9_10_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "1300", Msg1),
	Msg3 = erl8583_message:set(8, "1", Msg2),	
	Msg4 = erl8583_message:set(9, "2", Msg3),	
	Msg5 = erl8583_message:set(10, "3", Msg4),	
	[241, 243, 240, 240, 240, 241, 195, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 241, 240, 240, 240, 240, 240, 240, 240, 242, 240, 240, 240, 240, 240, 240, 240, 243] =
		erl8583_marshaller_ebcdic:marshal(Msg5).

fields_11_12_13_14_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(11, "1234", Msg2),	
	Msg4 = erl8583_message:set(12, "150755", Msg3),	
	Msg5 = erl8583_message:set(13, "2012", Msg4),
	Msg6 = erl8583_message:set(14, "1206", Msg5),
	[240, 242, 240, 240, 240, 240, 243, 195, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 240, 241, 242, 243, 244, 241, 245, 240, 247, 245, 245, 242, 240, 241, 242, 241, 242, 240, 246]
		= erl8583_marshaller_ebcdic:marshal(Msg6).

marshal_field_test() ->
	[241, 245, 243, 247, 240, 240, 241, 242, 243, 244, 245, 246, 241, 242, 243, 244, 245] = 
		erl8583_marshaller_ebcdic:marshal_field(2, "370012345612345", erl8583_fields),
	[240, 240, 244, 240, 240, 240] = erl8583_marshaller_ebcdic:marshal_field(3, "004000", erl8583_fields).

unmarshal_field_test() ->
	MarshalledField2 = [241, 245, 243, 247, 240, 240, 241, 242, 243, 244, 245, 246, 241, 242, 243, 244, 245],
	MarshalledRest = lists:seq(1, 40), % Non EBCDIC characters.
	Marshalled = MarshalledField2 ++ MarshalledRest,
	{"370012345612345", MarshalledRest} = erl8583_marshaller_ebcdic:unmarshal_field(2, Marshalled, erl8583_fields).

 