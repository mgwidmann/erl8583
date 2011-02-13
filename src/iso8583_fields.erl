%% Author: carl
%% Created: 12 Feb 2011
%% Description: TODO: Add description to iso8583_fields
-module(iso8583_fields).

%%
%% Include files
%%
-include("field_defines.hrl").

%%
%% Exported Functions
%%
-export([get_encoding/1]).

%%
%% API Functions
%%
get_encoding(?MTI) ->
	{n, fixed, 4};
get_encoding(?PAN) ->
	{n, llvar, 19};
get_encoding(3) ->
	{n, fixed, 6}.


%%
%% Local Functions
%%

