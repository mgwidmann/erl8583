%% Author: carl
%% Created: 27 Feb 2011
%% Description: TODO: Add description to encoding_rules
-module(encoding_rules).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([behaviour_info/1]).

%%
%% API Functions
%%
behaviour_info(callbacks) ->
	[{get_encoding, 1}];
behaviour_info(_Other) ->
	undefined.



%%
%% Local Functions
%%

