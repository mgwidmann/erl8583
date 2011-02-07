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
xml_marshall_empty_test() ->
	IsoMsg = iso8583_message:new(),
	Marshalled = xml_marshaller:marshall(IsoMsg),
	IsoMsg = xml_unmarshaller:unmarshall(Marshalled).

xml_marshall_simple_test() ->
	IsoMsg1 = iso8583_message:new(),
	IsoMsg2 = iso8583_message:set(0, "0200", IsoMsg1),
	Marshalled = xml_marshaller:marshall(IsoMsg2),
	IsoMsg2 = xml_unmarshaller:unmarshall(Marshalled).

xml_marshall_complex_test() ->
	BitMap = iso8583_message:from_list([{1, "foo"}, {2, "bar"}]),
	IsoMsg = iso8583_message:from_list([{1, "0200"}, {3, "333333"}, {48, BitMap} ]),
	Marshalled = xml_marshaller:marshall(IsoMsg),
	IsoMsg = xml_unmarshaller:unmarshall(Marshalled).

%%
%% Local Functions
%%

