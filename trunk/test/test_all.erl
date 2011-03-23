%% Author: carl
%% Created: 20 Mar 2011
%% Description: Runs all tests in the test directory
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
	{ok, Files} = file:list_dir("./tbin"),
	BeamPred = fun(F) -> is_beam_file(F) end,
	BeamFiles = lists:sort(lists:filter(BeamPred, Files)),
	TestModules = [erlang:list_to_atom(lists:sublist(F, 1, length(F)-5)) || F <- BeamFiles],
	TestModules2 = [Mod || Mod <- TestModules, Mod =/= test_all],
	[run_tests(Module) || Module <- TestModules2].

%%
%% Local Functions
%%
is_beam_file(FileName) when length(FileName) >= 5 ->
	Length = length(FileName),
	lists:sublist(FileName, Length-4, 5) =:= ".beam";
is_beam_file(_FileName) ->
	false.

run_tests(Module) ->
	io:format("Running ~p:~n", [Module]),
	Module:test().
