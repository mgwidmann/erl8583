%% Author: carl
%% Created: 19 Feb 2011
%% Description: TODO: Add description to binary_unmarshaller
-module(binary_unmarshaller).

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
	IsoMsg1 = iso8583_message:new(),
	{MtiBin, _} = split_binary(Msg, 2),
	Mti = string_utils:binary_to_ascii_hex(MtiBin),
	iso8583_message:set(0, Mti, IsoMsg1).



%%
%% Local Functions
%%

