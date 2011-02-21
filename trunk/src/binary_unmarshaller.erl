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
	{MtiBin, Rest} = split_binary(Msg, 2),
	Mti = convert:binary_to_ascii_hex(MtiBin),
	IsoMsg2 = iso8583_message:set(0, Mti, IsoMsg1),
	{FieldIds, _Fields} = extract_fields(Rest),
	populate_fields(FieldIds, IsoMsg2).
	


%%
%% Local Functions
%%
extract_fields(<<>>) ->
	{[], <<>>};
extract_fields(Message) ->
	BitMapLength = get_bit_map_length(Message),
	{BinaryBitMap, Fields} = split_binary(Message, BitMapLength),
	BitMap = binary_to_list(BinaryBitMap),
	extract_fields(BitMap, 0, 8, {[], Fields}).

get_bit_map_length(_Message) ->
	8.

extract_fields([], _Offset, _Index, {FieldIds, Fields}) ->
	Ids = lists:sort(FieldIds),
	{[Id || Id <- Ids, Id rem 64 =/= 1], Fields};
extract_fields([_Head|Tail], Offset, 0, {FieldIds, Fields}) ->
	extract_fields(Tail, Offset+1, 8, {FieldIds, Fields});
extract_fields([Head|Tail], Offset, Index, {FieldIds, Fields}) ->
	case Head band (1 bsl (Index-1)) of
		0 ->
			extract_fields([Head|Tail], Offset, Index-1, {FieldIds, Fields});
		_ ->
			extract_fields([Head|Tail], Offset, Index-1, {[Offset*8+9-Index|FieldIds], Fields})
	end.

populate_fields([], Msg) ->
	Msg;
populate_fields([Id|Tail], Msg) ->
	UpdatedMsg = iso8583_message:set(Id, "Value", Msg),
	populate_fields(Tail, UpdatedMsg).
