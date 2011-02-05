%% Author: carl
%% Created: 05 Feb 2011
%% Description: TODO: Add description to test_iso8583_bit_field
-module(test_iso8583_bit_field).

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
new_test() ->
	{iso8583_bit_field, _} = iso8583_bit_field:new(10).
								


%%
%% Local Functions
%%

