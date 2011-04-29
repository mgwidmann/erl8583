-module(example_7_encoding_rules).

-export([get_encoding/1]).

get_encoding(2) ->
	{ans, llvar, 32};
get_encoding(3) ->
	{ans, fixed, 48};
get_encoding(12) ->
	{ans, llvar, 25};
get_encoding(13) ->
	{ans, fixed, 17};
get_encoding(14) ->
	{ans, fixed, 8};
get_encoding(20) ->
	{n, fixed, 8}.
