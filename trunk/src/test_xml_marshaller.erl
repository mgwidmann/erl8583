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
xml_marshall_test() ->
	IsoMsg = iso8583_message:new(),
	Marshalled = xml_marshaller:marshall(IsoMsg),
	IsoMsg = xml_unmarshaller:unmarshall(Marshalled).


%%
%% Local Functions
%%

