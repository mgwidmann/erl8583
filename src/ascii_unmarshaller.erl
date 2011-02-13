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
	IsoMsg1 = iso8583_message:new(),
	{Mti, Rest} = lists:split(4, Msg),
	IsoMsg2 = iso8583_message:set(0, Mti, IsoMsg1),
	{FieldIds, Fields} = extract_fields(Rest),
	decode_fields(FieldIds, Fields, IsoMsg2).


%%
%% Local Functions
%%
extract_fields([]) ->
	{[], []};
extract_fields(Message) ->
	{AsciiBitMap, Fields} = lists:split(16, Message),
	BitMap = string_utils:ascii_hex_to_string(AsciiBitMap),
	extract_fields(BitMap, 0, 8, {[], Fields}).

extract_fields([], _Offset, _Index, {FieldIds, Fields}) ->
	{lists:sort(FieldIds), Fields};
extract_fields([_Head|Tail], Offset, 0, {FieldIds, Fields}) ->
	extract_fields(Tail, Offset+1, 8, {FieldIds, Fields});
extract_fields([Head|Tail], Offset, Index, {FieldIds, Fields}) ->
	case Head band (1 bsl (Index-1)) of
		0 ->
			extract_fields([Head|Tail], Offset, Index-1, {FieldIds, Fields});
		_ ->
			extract_fields([Head|Tail], Offset, Index-1, {[Offset*8+9-Index|FieldIds], Fields})
	end.
			
decode_fields([], _, Result) ->
	Result;
decode_fields([Field|Tail], Fields, Result) ->
	Encoding = iso8583_fields:get_encoding(Field),
	{Value, UpdatedFields} = decode_field(Encoding, Fields),
	UpdatedResult = iso8583_message:set(Field, Value, Result),
	decode_fields(Tail, UpdatedFields, UpdatedResult).
	
decode_field({n, llvar, _}, Fields) ->
	{N, Rest} = lists:split(2, Fields),
	Length = list_to_integer(N),
	{Value, RemainingFields} = lists:split(Length, Rest),
	{Value, RemainingFields};
decode_field({n, fixed, Length}, Fields) ->
	lists:split(Length, Fields).
	