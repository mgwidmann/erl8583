%% Author: carl
%% Created: 12 Feb 2011
%% Description: TODO: Add description to iso8583_fields
-module(iso8583_fields).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([get_encoding/1]).

%%
%% API Functions
%%
get_encoding(0) ->
	{n, fixed, 4};
get_encoding(2) ->
	{n, llvar, 19};
get_encoding(3) ->
	{n, fixed, 6}.


%%
%% Local Functions
%%

