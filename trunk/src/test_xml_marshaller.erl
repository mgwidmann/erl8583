%% Author: carl
%% Created: 06 Feb 2011
%% Description: TODO: Add description to test_xml_marshaller
-module(test_xml_marshaller).

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

%% Check that marshalling does the opposite of unmarshalling.
xml_marshal_empty_test() ->
	IsoMsg = erl8583_message:new(),
	Marshalled = erl8583_marshaller_xml:marshal(IsoMsg),
	IsoMsg = erl8583_marshaller_xml:unmarshal(Marshalled).

xml_marshal_simple_test() ->
	IsoMsg1 = erl8583_message:new(),
	IsoMsg2 = erl8583_message:set(0, "0200", IsoMsg1),
	Marshalled = erl8583_marshaller_xml:marshal(IsoMsg2),
	IsoMsg2 = erl8583_marshaller_xml:unmarshal(Marshalled).

xml_marshal_complex_test() ->
	BitMap = erl8583_message:from_list([{1, "foo"}, {2, "bar"}]),
	IsoMsg = erl8583_message:from_list([{1, "0200"}, {3, "333333"}, {48, BitMap} ]),
	Marshalled = erl8583_marshaller_xml:marshal(IsoMsg),
	IsoMsg = erl8583_marshaller_xml:unmarshal(Marshalled).

xml_marshal_with_attributes_test() ->
	IsoMsg = erl8583_message:new([{"outgoing", "true"}]),
	Marshalled = erl8583_marshaller_xml:marshal(IsoMsg),
	IsoMsg = erl8583_marshaller_xml:unmarshal(Marshalled).

xml_marshal_complex_attributes_test() ->
	BitMap = erl8583_message:from_list([{1, "foo"}, {2, "bar"}]),
	BitMap2 = erl8583_message:set_attributes([{"foo","bar"},{"hello","world"}], BitMap),
	IsoMsg = erl8583_message:from_list([{1, "0200"}, {3, "333333"}, {48, BitMap2} ]),
	Marshalled = erl8583_marshaller_xml:marshal(IsoMsg),
	IsoMsg = erl8583_marshaller_xml:unmarshal(Marshalled).

xml_marshal_binary_test() ->
	IsoMsg1 = erl8583_message:new(),
	IsoMsg2 = erl8583_message:set(0, "0200", IsoMsg1),
	IsoMsg3 = erl8583_message:set(52, <<1,2,3,4,5,6,7,255>>, IsoMsg2),
	Marshalled = erl8583_marshaller_xml:marshal(IsoMsg3),
	IsoMsg3 = erl8583_marshaller_xml:unmarshal(Marshalled).
	
%%
%% Local Functions
%%

