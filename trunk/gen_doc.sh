#!/usr/bin/env escript

main(_) ->
	io:format("Generating edocs~n"),
	{ok, Files} = file:list_dir("./src"),
	ErlPred = fun(F) -> is_erl_file(F) end,
	ErlFiles = lists:filter(ErlPred, Files),
	TestPred = fun(F) -> not is_test_file(F) end,
	Files2 = lists:sort(lists:filter(TestPred, ErlFiles)),
	Modules = ["src/" ++ F || F <- Files2],
        edoc:files(Modules, [{dir, "doc"}]).

is_erl_file(FileName) when length(FileName) >= 4 ->
	Length = length(FileName),
	lists:sublist(FileName, Length-3, 4) =:= ".erl";
is_erl_file(_FileName) ->
	false.

is_test_file(FileName) when length(FileName) >= 5 ->
	lists:sublist(FileName, 1, 5) =:= "test_";
is_test_file(_FileName) ->
	false.

