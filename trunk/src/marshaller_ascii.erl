%% Author: carl
%% Created: 12 Feb 2011
%% Description: TODO: Add description to ascii_marshaller
-module(marshaller_ascii).

%%
%% Include files
%%


%%
%% Exported Functions
%%
-export([marshal/1, 
		 marshal/2, 
		 unmarshal/1, 
		 unmarshal/2, 
		 construct_bitmap/1, 
		 extract_fields/1]).

%%
%% API Functions
%%
marshal(Msg) ->
	marshal(Msg, marshaller_ascii_field).

marshal(Msg, FieldMarshaller) ->
	Mti = iso8583_message:get(0, Msg),
	[0|Fields] = iso8583_message:get_fields(Msg),
	Mti ++ construct_bitmap(Fields) ++ encode(Fields, Msg, FieldMarshaller).
	
unmarshal(Msg) ->
	unmarshal(Msg, marshaller_ascii_field).

unmarshal(Msg, FieldMarshaller) ->
	IsoMsg1 = iso8583_message:new(),
	{Mti, Rest} = lists:split(4, Msg),
	IsoMsg2 = iso8583_message:set(0, Mti, IsoMsg1),
	{FieldIds, Fields} = extract_fields(Rest),
	decode_fields(FieldIds, Fields, IsoMsg2, FieldMarshaller).

construct_bitmap([]) ->
	[];
construct_bitmap(Fields) ->
	NumBitMaps = (lists:max(Fields) + 63) div 64,
	ExtensionBits = [Bit * 64 - 127 || Bit <- lists:seq(2, NumBitMaps)],
	BitMap = lists:duplicate(NumBitMaps * 8, 0),
	convert:string_to_ascii_hex(construct_bitmap(lists:sort(ExtensionBits ++ Fields), BitMap)).

extract_fields([]) ->
	{[], []};
extract_fields(Message) ->
	BitMapLength = get_bit_map_length(Message),
	{AsciiBitMap, Fields} = lists:split(BitMapLength, Message),
	BitMap = convert:ascii_hex_to_string(AsciiBitMap),
	extract_fields(BitMap, 0, 8, {[], Fields}).

%%
%% Local Functions
%%
construct_bitmap([], Result) ->
	Result;
construct_bitmap([Field|Tail], Result) when Field > 0 ->
	ByteNum = (Field - 1) div 8,
	BitNum = 7 - ((Field - 1) rem 8),
	{Left, Right} = lists:split(ByteNum, Result),
	[ToUpdate | RightRest] = Right,
	construct_bitmap(Tail, Left ++ ([ToUpdate + (1 bsl BitNum)]) ++ RightRest).

encode(Fields, Msg, FieldMarshaller) ->
	lists:reverse(encode(Fields, Msg, [], FieldMarshaller)).

encode([], _Msg, Result, _FieldMarshaller) ->
	Result;
encode([FieldId|Tail], Msg, Result, FieldMarshaller) ->
	Value = iso8583_message:get(FieldId, Msg),
	EncodedValue = FieldMarshaller:encode_field(FieldId, Value),
	encode(Tail, Msg, lists:reverse(EncodedValue) ++ Result, FieldMarshaller).
 
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
			
decode_fields([], _, Result, _FieldMarshaller) ->
	Result;
decode_fields([FieldId|Tail], Fields, Result, FieldMarshaller) ->
	{Value, UpdatedFields} = FieldMarshaller:decode_field(FieldId, Fields),
	UpdatedResult = iso8583_message:set(FieldId, Value, Result),
	decode_fields(Tail, UpdatedFields, UpdatedResult, FieldMarshaller).
	
get_bit_map_length(Msg) ->
	get_bit_map_length(Msg, 16).

get_bit_map_length(Msg, Length) ->
	[HexDig1, HexDig2|_Tail] = Msg,
	<<Byte>> = convert:ascii_hex_to_binary([HexDig1, HexDig2]),
	case (Byte band 128) of
		0 ->
			Length;
		_ ->
			{_Msg1, Msg2} = lists:split(16, Msg),
			get_bit_map_length(Msg2, Length+16)
	end.
