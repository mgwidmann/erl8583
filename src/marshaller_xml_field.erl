%% Author: carl
%% Created: 06 Mar 2011
%% Description: TODO: Add description to marshaller_xml_field
-module(marshaller_xml_field).

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
marshal(FieldId, Value) when is_list(Value)->
	Id = integer_to_list(FieldId),
	"<field id=\"" ++ Id ++ "\" value=\"" ++ Value ++ "\" />";
marshal(FieldId, Value) when is_binary(Value) ->
	Id = integer_to_list(FieldId),
	"<field id=\"" ++ 
		Id ++ 
		"\" value=\"" ++ 
		convert:binary_to_ascii_hex(Value) ++
		"\" type=\"binary\" />";	
marshal(FieldId, Value) ->
	Id = integer_to_list(FieldId),
	"<isomsg id=\"" ++ 
		Id ++ 
		"\"" ++
		encode_attributes(iso8583_message:get_attributes(Value)) ++
		">" ++
		marshaller_xml:marshal_fields(iso8583_message:to_list(Value), "", ?MODULE) ++ 
		"</isomsg>".

%%
%% Local Functions
%%
encode_attributes(List) ->
	encode_attributes(List, "").

encode_attributes([], Result) ->
	Result;
encode_attributes([{Key, Value} | Tail], Result) ->
	encode_attributes(Tail, " " ++ Key ++ "=\"" ++ Value ++ "\"" ++  Result).

