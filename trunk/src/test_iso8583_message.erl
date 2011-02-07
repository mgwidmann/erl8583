%% Author: carl
%% Created: 20 Jan 2011
%% Description: TODO: Add description to test_iso8583_message
-module(test_iso8583_message).

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

%%
%% Tests
%%

%% Check that "new" creates the expected tuple.
new_test() ->
	{iso8583_message, _, _} = iso8583_message:new().

%% Check that we can add a field with ID 0
set_test() ->
	Message = iso8583_message:new(),
	iso8583_message:set(0, "0200", Message).

%% Check that we can't add a field with a negative ID.
set_negative_id_test() ->
	Message = iso8583_message:new(),
	?assertError(_, iso8583_message:set_field(-1, "0200", Message)).
	
%% Check that we can't add a field more than once.
set_field_twice_test() ->
	Message = iso8583_message:new(),
	Message2 = iso8583_message:set(0, "0200", Message),
	?assertError(_, iso8583_message:set(0, "0210", Message2)).

set_field_not_integer_test() ->
	Message = iso8583_message:new(),
	?assertError
	(_, iso8583_message:set(foo, "0210", Message)).

set_negative_field_test() ->
	Message = iso8583_message:new(),
	?assertError(_, iso8583_message:set(-1, "0210", Message)).
	
%% Error should be thrown if we read an unset field.
get_unset_field_test() ->
	Message = iso8583_message:new(),
	?assertError(_, iso8583_message:get(0, Message)).

%% Should be able to read a set field.
get_field_test() ->
	Message = iso8583_message:new(),
	Message2 = iso8583_message:set(0, "0200", Message),
	?assertEqual("0200", iso8583_message:get(0, Message2)).

%% Test that we can get fields.
get_fields_test() ->
	Message = iso8583_message:new([{mapper, ?MODULE}]),
	Message2 = iso8583_message:set(180, "hello", Message),
	Message3 = iso8583_message:set(0, "0200", Message2),
	?assertEqual([0, 180], iso8583_message:get_fields(Message3)).

%% Test to_list.
to_list_test() ->
	Message = iso8583_message:new(),
	Message2 = iso8583_message:set(0, "0200", Message),
	Message3 = iso8583_message:set(39, 0, Message2),
	[{0, "0200"}, {39, 0}] = iso8583_message:to_list(Message3).

from_list_test() ->
	Message = iso8583_message:from_list([{0, "0200"}, {39, 0}]),
	{iso8583_message, _, _} = Message,
	[{0, "0200"}, {39, 0}] = iso8583_message:to_list(Message).

get_attributes_test() ->
	Message = iso8583_message:new([{"foo", "bar"}, {"hello", "world"}]),
	[{"foo", "bar"}, {"hello", "world"}] = iso8583_message:get_attributes(Message).

set_attributes_test() ->
	Msg = iso8583_message:new(),
	UpdatedMsg = iso8583_message:set_attributes([{"foo", "bar"}, {"hello", "world"}], Msg),
	[{"foo", "bar"}, {"hello", "world"}] = iso8583_message:get_attributes(UpdatedMsg).
	