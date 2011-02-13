%% Author: carl
%% Created: 13 Feb 2011
%% Description: TODO: Add description to ascii_unmarshaller
-module(ascii_unmarshaller).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([unmarshall/1, unmarshall/2]).

%%
%% API Functions
%%
unmarshall(Msg) ->
	unmarshall(Msg, iso8583_fields).

unmarshall(Msg, _EncodingRules) ->
	IsoMsg = iso8583_message:new(),
	iso8583_message:set(0, Msg, IsoMsg).



%%
%% Local Functions
%%

