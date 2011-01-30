%% Author: carl
%% Created: 25 Jan 2011
%% Description: TODO: Add description to field_validator
-module(field_validator).

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
	[{validate_field, 2}];
behaviour_info(_Other) ->
	undefined.



%%
%% Local Functions
%%

