%% Author: carl
%% Created: 12 Feb 2011
%% Description: TODO: Add description to ascii_marshaller
-module(ascii_marshaller).

%%
%% Include files
%%


%%
%% Exported Functions
%%
-export([marshall/1]).

%%
%% API Functions
%%
marshall(Msg) ->
	Mti = iso8583_message:get(0, Msg),
	string_utils:string_to_ascii_hex(Mti).



%%
%% Local Functions
%%
