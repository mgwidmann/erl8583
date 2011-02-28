%% Author: carl
%% Created: 28 Feb 2011
%% Description: TODO: Add description to custom_marshaller
-module(custom_marshaller).

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
	[{marshal, 2},
	 {unmarshal, 2}];
behaviour_info(_Other) ->
	undefined.



%%
%% Local Functions
%%

