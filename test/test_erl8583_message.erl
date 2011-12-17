%% Author: carl
%% Created: 20 Jan 2011
%% Description: TODO: Add description to test_iso8583_message
-module(test_erl8583_message).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("../include/erl8583_types.hrl").

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
	#iso8583_message{attributes=[]} = erl8583_message:new().

%% Check that we can add a field with ID 0
set_test() ->
	Message = erl8583_message:new(),
	erl8583_message:set(0, "0200", Message).

%% Check that we can't add a field with a negative ID.
set_negative_id_test() ->
	Message = erl8583_message:new(),
	?assertError(_, erl8583_message:set_field(-1, "0200", Message)).
	
set_field_not_integer_test() ->
	Message = erl8583_message:new(),
	?assertError(_, erl8583_message:set(foo, "0210", Message)).

set_negative_field_test() ->
	Message = erl8583_message:new(),
	?assertError(_, erl8583_message:set(-1, "0210", Message)).
	
%% Error should be thrown if we read an unset field.
get_unset_field_test() ->
	Message = erl8583_message:new(),
	?assertError(_, erl8583_message:get(0, Message)).

%% Should be able to read a set field.
get_field_test() ->
	Message = erl8583_message:new(),
	Message2 = erl8583_message:set(0, "0200", Message),
	?assertEqual("0200", erl8583_message:get(0, Message2)).

%% Test that we can get fields.
get_fields_test() ->
	Message = erl8583_message:new(),
	Message2 = erl8583_message:set(180, "hello", Message),
	Message3 = erl8583_message:set(0, "0200", Message2),
	?assertEqual([0, 180], erl8583_message:get_fields(Message3)).

from_list_test() ->
	Message = erl8583_message:from_list([{0, "0200"}, {39, 0}]),
	#iso8583_message{attributes=[]} = Message.
	%[{0, "0200"}, {39, 0}] = erl8583_message:to_list(Message).

get_attributes_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set_attribute("foo", "bar", Msg1),
	Msg3 = erl8583_message:set_attribute("hello", "world", Msg2),
	[{"hello", "world"}, {"foo", "bar"}] = erl8583_message:get_attributes(Msg3).

set_attributes_test() ->
	Msg = erl8583_message:new(),
	UpdatedMsg = erl8583_message:set_attributes([{"foo", "bar"}, {"hello", "world"}], Msg),
	[{"foo", "bar"}, {"hello", "world"}] = erl8583_message:get_attributes(UpdatedMsg).

update_test() ->
	Msg = erl8583_message:new(),
	UpdatedMsg = erl8583_message:set(3, "foo", Msg),
	"foo" = erl8583_message:get(3, UpdatedMsg),
	ChangedMsg = erl8583_message:set(3, "bar", UpdatedMsg),
	"bar" = erl8583_message:get(3, ChangedMsg).

clone_fields_test() ->
	Message = erl8583_message:new(),
	Message2 = erl8583_message:set(10, "hello", Message),
	Message3 = erl8583_message:set(0, "0200", Message2),
	Message4 = erl8583_message:set(2, "hello2", Message3),
	Clone1 = erl8583_message:clone_fields([0, 2, 10], Message4),
	[0, 2, 10] = erl8583_message:get_fields(Clone1),
	Clone2 = erl8583_message:clone_fields([0, 10], Message4),
	[0, 10] = erl8583_message:get_fields(Clone2),
	"hello" = erl8583_message:get(10, Clone2),
	"0200" = erl8583_message:get(0, Clone2).

get_list1_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:set(4, "baz", Message1),
	"baz" = erl8583_message:get([4], Message2),
	?assertError(_, erl8583_message:get([[4]], Message2)).
	
get_list2_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:set(4, "baz", erl8583_message:new()),
	Message3 = erl8583_message:set(2, Message2, Message1),
	Message2 = erl8583_message:get([2], Message3),
	"baz" = erl8583_message:get([2, 4], Message3).

set_list1_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:set([4], "baz", Message1),
	"baz" = erl8583_message:get([4], Message2),
	"baz" = erl8583_message:get(4, Message2).

set_list2_test() ->
	Message1 = erl8583_message:set(5, erl8583_message:new(), erl8583_message:new()),
	Message2 = erl8583_message:set([5, 2], "foobar", Message1),
	"foobar" = erl8583_message:get([5, 2], Message2).

update_list1_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:set([4], "baz", Message1),
	Message3 = erl8583_message:set([4], "foobar", Message2),
	"foobar" = erl8583_message:get([4], Message3).

update_list2_test() ->
	Message1 = erl8583_message:set(5, erl8583_message:new(), erl8583_message:new()),
	Message2 = erl8583_message:set([5, 2], "foobar", Message1),
	"foobar" = erl8583_message:get([5, 2], Message2),
	Message3 = erl8583_message:set([5, 2], "foobar2", Message2),
	"foobar2" = erl8583_message:get([5, 2], Message3).

set_list3_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:set([5, 2], "foo", Message1),
	Message3 = erl8583_message:set([5, 3, 1], "bar", Message2),
	SubMessage = erl8583_message:get(5, Message3),
	[2, 3] = erl8583_message:get_fields(SubMessage),
	"foo" = erl8583_message:get(2, SubMessage),
	"bar" = erl8583_message:get([3, 1], SubMessage).

update_list3_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:set([5, 2], "foo", Message1),
	Message3 = erl8583_message:set([5, 3, 1], "bar", Message2),
	SubMessage = erl8583_message:get(5, Message3),
	[2, 3] = erl8583_message:get_fields(SubMessage),
	"foo" = erl8583_message:get(2, SubMessage),
	"bar" = erl8583_message:get([3, 1], SubMessage).

get_numeric_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:set([5, 2], "0123", Message1),
	Message3 = erl8583_message:set(6, "77", Message2),
	123 = erl8583_message:get_numeric([5, 2], Message3),
	77 = erl8583_message:get_numeric(6, Message3).
	
set_numeric_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:set_numeric([5, 2], 123, 12, Message1),
	Message3 = erl8583_message:set_numeric(6, 77, 2, Message2),
	"000000000123" = erl8583_message:get([5, 2], Message3),
	"77" = erl8583_message:get(6, Message3).
	
update_numeric_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:set_numeric([5, 2], 123, 12, Message1),
	Message3 = erl8583_message:set_numeric([5, 2], 77, 3, Message2),
	"077" = erl8583_message:get([5, 2], Message3).
	
is_message_test() ->
	false = erl8583_message:is_message(3),
	true = erl8583_message:is_message(#iso8583_message{}).

clone_with_attributes_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set_attribute("foo", "bar", Msg1),
	Msg3 = erl8583_message:clone_fields([], Msg2),
	[{"foo", "bar"}] = erl8583_message:get_attributes(Msg3).

get_attribute_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set_attribute("foo", "bar", Msg1),
	Msg3 = erl8583_message:set_attribute("bar", "baz", Msg2),
	"bar" = erl8583_message:get_attribute("foo", Msg3),
	"baz" = erl8583_message:get_attribute("bar", Msg3).

set_attribute_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set_attribute("baz", "3", Msg1),
	"3" = erl8583_message:get_attribute("baz", Msg2).

update_attribute_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set_attribute("foo", "bar", Msg1),
	Msg3 = erl8583_message:set_attribute("bar", "baz", Msg2),
	Msg4 = erl8583_message:set_attribute("baz", "3", Msg3),
	Msg5 = erl8583_message:set_attribute("foo", "3", Msg4),
	"3" = erl8583_message:get_attribute("baz", Msg5),
	"3" = erl8583_message:get_attribute("foo", Msg5).

remove_1_test() ->
	Message = erl8583_message:new(),
	Message2 = erl8583_message:set(10, "hello", Message),
	Message3 = erl8583_message:set(0, "0200", Message2),
	Message4 = erl8583_message:set(2, "hello2", Message3),
	Message5 = erl8583_message:remove(2, Message4),
	[0, 10] = erl8583_message:get_fields(Message5).

remove_2_test() ->
	Message = erl8583_message:new(),
	Message2 = erl8583_message:set([10, 1], "hello", Message),
	Message3 = erl8583_message:set([10, 3], "0200", Message2),
	Message4 = erl8583_message:set(2, "hello2", Message3),
	Message5 = erl8583_message:remove([10, 1], Message4),
	[2, 10] = erl8583_message:get_fields(Message5),
	?assertError(_, erl8583_message:get([10, 1], Message5)).
	
% Removing all the subfields, should remove the field.
remove_3_test() ->
	Message = erl8583_message:new(),
	Message2 = erl8583_message:set([10, 1], "hello", Message),
	Message3 = erl8583_message:set([10, 3], "0200", Message2),
	Message4 = erl8583_message:set(2, "hello2", Message3),
	Message5 = erl8583_message:remove([10, 1], Message4),
	Message6 = erl8583_message:remove([10, 3], Message5),
	[2] = erl8583_message:get_fields(Message6).

% Removing a non-existent field is not an error.
remove_4_test() ->
	Message = erl8583_message:new(),
	erl8583_message:remove(1, Message),
	erl8583_message:remove([1], Message),	
	erl8583_message:remove([1, 2], Message).
