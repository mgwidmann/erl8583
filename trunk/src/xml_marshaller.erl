%% Author: carl
%% Created: 06 Feb 2011
%% Description: TODO: Add description to xml_marshaller
-module(xml_marshaller).

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
marshall(IsoMsg) ->
	"<isomsg>" ++ marshall_fields(iso8583_message:to_list(IsoMsg), []) ++ "</isomsg>\n".


%%
%% Local Functions
%%
marshall_fields([], Result) ->
	Result;
marshall_fields([{K, V}|Tail], Result) ->
	Id = integer_to_list(K),
	marshall_fields(Tail, "<field id=\"" ++ Id ++ "\" value=\"" ++ V ++ "\" />" ++ Result).
