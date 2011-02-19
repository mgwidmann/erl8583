%% Author: carl
%% Created: 19 Feb 2011
%% Description: TODO: Add description to bin_marshaller
-module(binary_marshaller).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([marshall/1, marshall/2]).

%%
%% API Functions
%%
marshall(Msg) ->
	marshall(Msg, iso8583_fields).

marshall(Msg, _EncodingRules) ->
	Mti = iso8583_message:get(0, Msg),
	string_utils:ascii_hex_to_binary(Mti).


%%
%% Local Functions
%%

