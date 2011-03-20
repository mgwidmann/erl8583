%% Author: carl
%% Created: 20 Mar 2011
%% Description: TODO: Add description to test_all
-module(test_all).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([test/0]).

%%
%% API Functions
%%
test() ->
	{ok, Files} = file:list_dir("./ebin"),
	BeamPred = fun(F) -> is_beam_file(F) end,
	BeamFiles = lists:filter(BeamPred, Files),
	TestPred = fun(F) -> is_test_file(F) end,
	TestFiles = lists:filter(TestPred, BeamFiles),
	TestModules = [erlang:list_to_atom(lists:sublist(F, 1, length(F)-5)) || F <- TestFiles],
	TestModules2 = [Mod || Mod <- TestModules, Mod =/= test_all],
	[Module:test() || Module <- TestModules2].

%%
%% Local Functions
%%
is_beam_file(FileName) when length(FileName) >= 5 ->
	Length = length(FileName),
	lists:sublist(FileName, Length-4, 5) =:= ".beam";
is_beam_file(_FileName) ->
	false.

is_test_file(FileName) when length(FileName) >= 5 ->
	lists:sublist(FileName, 1, 5) =:= "test_";
is_test_file(_FileName) ->
	false.

