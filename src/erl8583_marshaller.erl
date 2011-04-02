%% Author: carl
%% Created: 02 Apr 2011
%% Description: TODO: Add description to erl8583_marshaller
-module(erl8583_marshaller).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([marshal/2]).

%%
%% API Functions
%%
marshal(Message, Options) ->
	[{field_marshaller, Module}] = Options,
	Module:marshal_field(0, erl8583_message:get(0, Message), erl8583_fields).




%%
%% Local Functions
%%

