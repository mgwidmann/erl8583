%% Author: carl
%% Created: 24 Jan 2011
%% Description: TODO: Add description to field_id_mapper
-module(field_id_mapper).

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
	[{map_atom_to_index, 1}];
behaviour_info(_Other) ->
	undefined.


%%
%% Local Functions
%%
