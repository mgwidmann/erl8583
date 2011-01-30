%% Author: carl
%% Created: 20 Jan 2011
%% Description: TODO: Add description to test_iso8583_message
-module(test_iso8583_message).

-behaviour(field_id_mapper).
-behaviour(field_validator).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("iso8583_defines.hrl").

%%
%% Exported Functions
%%
-export([map_atom_to_id/1, validate_field/2]).

%%
%% API Functions
%%

%% This class provides self-shunting for testing.
map_atom_to_id(foo) ->
	180;
map_atom_to_id(bad_atom) ->
	bad_atom.

validate_field(1, _) ->
	ok;
validate_field(92, _) ->
	erlang:error(unexpected_field);
validate_field(_, _) ->
	ok.

%%
%% Tests
%%

%% Check that "new" creates the expected tuple.
new_test() ->
	{iso8583_message, _} = iso8583_message:new().

%% Check that we can add a field with ID 0
set_test() ->
	Message = iso8583_message:new(),
	iso8583_message:set(0, "0200", Message).

%% Check that we can't add a field with a negative ID.
set_negative_id_test() ->
	Message = iso8583_message:new(),
	?assertError(_, iso8583_message:set_field(-1, "0200", Message)).
	
%% Check that we can't add a field with a positive ID greater than MAX_FIELD_ID.
set_bad_positive_id_test() ->
	Message = iso8583_message:new(),
	?assertError(_, iso8583_message:set_field(?MAX_FIELD_ID, "0200", Message)).
	
%% Check that we can't add a field more than once.
set_field_twice_test() ->
	Message = iso8583_message:new(),
	Message2 = iso8583_message:set(0, "0200", Message),
	?assertError(field_already_set, iso8583_message:set(0, "0210", Message2)).

%% Error should be thrown if we read an unset field.
get_unset_field_test() ->
	Message = iso8583_message:new(),
	?assertError(_, iso8583_message:get(0, Message)).

%% Should be able to read a set field.
get_field_test() ->
	Message = iso8583_message:new(),
	Message2 = iso8583_message:set(0, "0200", Message),
	?assertEqual("0200", iso8583_message:get(0, Message2)).

%% Test new with a mapper and set using the mapper.
new_with_mapper_set_test() ->
	Message = iso8583_message:new([{mapper, ?MODULE}]),
	Message2 = iso8583_message:set(foo, "hello", Message),
	?assertEqual("hello", iso8583_message:get(180, Message2)).

%% Test new with a mapper and get using the mapper.
new_with_mapper_get_test() ->
	Message = iso8583_message:new([{mapper, ?MODULE}]),
	Message2 = iso8583_message:set(180, "hello", Message),
	?assertEqual("hello", iso8583_message:get(foo, Message2)).

%% Test new with a bad option
new_with_bad_option_test() ->
	?assertError(_, iso8583_message:new([{bad_option, ?MODULE}])).

%% Test with a mapper that doesn't map an atom to an ID.
bad_atom_mapper_test() ->
	Message = iso8583_message:new([{mapper, ?MODULE}]),
	?assertError(_, iso8583_message:set(bad_atom, "hello", Message)).

%% Test with a validator that approves fields 0 and 1.
validator_test() ->
	Message = iso8583_message:new([{validator, ?MODULE}]),
	Message2 = iso8583_message:set(0, "0200", Message),
	{iso8583_message, _} = iso8583_message:set(1, "hello", Message2).

%% Test with a validator that rejects field 92.
validator_bad_field_test() ->
	Message = iso8583_message:new([{validator, ?MODULE}]),
	?assertError(unexpected_field, iso8583_message:set(92, "hello", Message)).
	
%% Test that we can get fields.
get_fields_test() ->
	Message = iso8583_message:new([{mapper, ?MODULE}]),
	Message2 = iso8583_message:set(foo, "hello", Message),
	Message3 = iso8583_message:set(0, "0200", Message2),
	?assertEqual([0, 180], iso8583_message:get_fields(Message3)).

%% Test to_list.
to_list_test() ->
	Message = iso8583_message:new(),
	Message2 = iso8583_message:set(0, "0200", Message),
	Message3 = iso8583_message:set(39, 0, Message2),
	[{0, "0200"}, {39, 0}] = iso8583_message:to_list(Message3).

	