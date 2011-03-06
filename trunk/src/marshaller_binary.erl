%% Author: carl
%% Created: 19 Feb 2011
%% Description: TODO: Add description to bin_marshaller
-module(marshaller_binary).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([marshal/1, marshal/2, unmarshal/1, unmarshal/2, construct_bitmap/1, extract_fields/1]).

%%
%% API Functions
%%
marshal(Msg) ->
	marshal(Msg, marshaller_binary_field).

marshal(Msg, FieldMarshaller) ->
	Mti = iso8583_message:get(0, Msg),
	MtiBits = convert:ascii_hex_to_binary(Mti),
	[0|Fields] = iso8583_message:get_fields(Msg),
	BitMap = construct_bitmap(Fields),
	EncodedFields = encode(Fields, Msg, FieldMarshaller),
	<< MtiBits/binary, BitMap/binary, EncodedFields/binary>>.

unmarshal(Msg) ->
	unmarshal(Msg, marshaller_binary_field).

unmarshal(Msg, FieldMarshaller) ->
	IsoMsg1 = iso8583_message:new(),
	{MtiBin, Rest} = split_binary(Msg, 2),
	Mti = convert:binary_to_ascii_hex(MtiBin),
	IsoMsg2 = iso8583_message:set(0, Mti, IsoMsg1),
	{FieldIds, Fields} = extract_fields(Rest),
	decode_fields(FieldIds, Fields, IsoMsg2, FieldMarshaller).

construct_bitmap([]) ->
	<<>>;
construct_bitmap(Fields) ->
	NumBitMaps = (lists:max(Fields) + 63) div 64,
	ExtensionBits = [Bit * 64 - 127 || Bit <- lists:seq(2, NumBitMaps)],
	BitMap = lists:duplicate(NumBitMaps * 8, 0),
	construct_bitmap(lists:sort(ExtensionBits ++ Fields), BitMap).

extract_fields(<<>>) ->
	{[], <<>>};
extract_fields(Message) ->
	BitMapLength = get_bit_map_length(Message),
	{BinaryBitMap, Fields} = split_binary(Message, BitMapLength),
	BitMap = binary_to_list(BinaryBitMap),
	extract_fields(BitMap, 0, 8, {[], Fields}).

%%
%% Local Functions
%%
construct_bitmap([], Result) ->
	list_to_binary(Result);
construct_bitmap([Field|Tail], Result) when Field > 0 ->
	ByteNum = (Field - 1) div 8,
	BitNum = 7 - ((Field - 1) rem 8),
	{Left, Right} = lists:split(ByteNum, Result),
	[ToUpdate | RightRest] = Right,
	construct_bitmap(Tail, Left ++ ([ToUpdate + (1 bsl BitNum)]) ++ RightRest).

encode(Fields, Msg, FieldMarshaller) ->
	encode(Fields, Msg, <<>>, FieldMarshaller).

encode([], _Msg, Result, _FieldMarshaller) ->
	Result;
encode([FieldId|Tail], Msg, Result, FieldMarshaller) ->
	Value = iso8583_message:get(FieldId, Msg),
	EncodedValue = FieldMarshaller:encode_field(FieldId, Value),
	encode(Tail, Msg, convert:concat_binaries(Result, EncodedValue), FieldMarshaller).

get_bit_map_length(Message) ->
	[Head|_Tail] = binary_to_list(Message),
	case Head >= 128 of
		false ->
			8;
		true ->
			{_, Rest} = erlang:split_binary(Message, 8),
			8 + get_bit_map_length(Rest)
	end.

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

decode_fields([], _, Result, _EncodingRules) ->
	Result;
decode_fields([FieldId|Tail], Fields, Result, FieldMarshaller) ->
	{Value, UpdatedFields} = FieldMarshaller:decode_field(FieldId, Fields),
	UpdatedResult = iso8583_message:set(FieldId, Value, Result),
	decode_fields(Tail, UpdatedFields, UpdatedResult, FieldMarshaller).
