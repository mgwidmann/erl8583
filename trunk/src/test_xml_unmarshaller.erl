%% Author: carl
%% Created: 29 Jan 2011
%% Description: TODO: Add description to test_xml_unmarshaller
-module(test_xml_unmarshaller).

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
xml_unmarshall_test() ->
	XmlMessage = "<isomsg>" ++
					 "<field id=\"0\" value=\"0800\"/>" ++
					 "<field id=\"3\" value=\"333333\"/>" ++
					 "</isomsg>",
	Message = xml_unmarshaller:unmarshall(XmlMessage),
	?assertEqual("0800", iso8583_message:get(0, Message)),
	?assertEqual("333333", iso8583_message:get(3, Message)),
	?assertEqual([0, 3], iso8583_message:get_fields(Message)).
	
xml_unmarshall_with_text_test() ->
	XmlMessage = "<isomsg>Some text" ++
					 "<field value=\"0800\" id=\"0\">more text</field>" ++
					 "<field id=\"3\" value=\"333333\"/>" ++
					 "</isomsg>",
	Message = xml_unmarshaller:unmarshall(XmlMessage),
	?assertEqual("0800", iso8583_message:get(0, Message)),
	?assertEqual("333333", iso8583_message:get(3, Message)),
	?assertEqual([0, 3], iso8583_message:get_fields(Message)).
	

%%
%% Local Functions
%%

