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

unmarshall(Msg, EncodingRules) ->
	IsoMsg1 = iso8583_message:new(),
	{MtiBin, Rest} = split_binary(Msg, 2),
	Mti = convert:binary_to_ascii_hex(MtiBin),
	IsoMsg2 = iso8583_message:set(0, Mti, IsoMsg1),
	{FieldIds, Fields} = extract_fields(Rest),
	decode_fields(FieldIds, Fields, IsoMsg2, EncodingRules).

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

decode_fields([], _, Result, _EncodingRules) ->
	Result;
decode_fields([Field|Tail], Fields, Result, EncodingRules) ->
	Encoding = EncodingRules:get_encoding(Field),
	{Value, UpdatedFields} = decode_field(Encoding, Fields),
	UpdatedResult = iso8583_message:set(Field, Value, Result),
	decode_fields(Tail, UpdatedFields, UpdatedResult, EncodingRules).
	
decode_field({n, llvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 1),
	N = convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, (N+1) div 2), 
	{convert:bcd_to_ascii_hex(ValueBin, N, "0"), Rest};
decode_field({n, lllvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 2),
	N = convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, (N+1) div 2), 
	{convert:bcd_to_ascii_hex(ValueBin, N, "0"), Rest};
decode_field({ns, llvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 1),
	N = convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, N), 
	{binary_to_list(ValueBin), Rest};
decode_field({ans, lllvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 2),
	N = convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, N), 
	{binary_to_list(ValueBin), Rest};
decode_field({n, fixed, Length}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, (Length + 1) div 2),
	case Length rem 2 of
		0 ->
			{convert:bcd_to_ascii_hex(NBin, Length, "0"), RestBin};
		1 ->
			[$0|AsciiHex] = convert:bcd_to_ascii_hex(NBin, Length+1, "0"),
			{AsciiHex, RestBin}
	end;
decode_field({an, fixed, Length}, Fields) ->
	{FieldBin, RestBin} = split_binary(Fields, Length),
	FieldStr = binary_to_list(FieldBin),
	{convert:pad_with_trailing_spaces(FieldStr, Length), RestBin};
decode_field({x_n, fixed, Length}, Fields) ->
	{FieldBin, RestBin} = split_binary(Fields, Length div 2 + 1),
	{<<X>>, Value} = split_binary(FieldBin, 1),
	ValueStr = convert:bcd_to_ascii_hex(Value, Length, "0"),
	case X =:= $C orelse X =:= $D of
		true ->
			{[X] ++ ValueStr, RestBin}
	end;
decode_field({z, llvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 1),
	N = convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, (N+1) div 2), 
	{convert:track2_to_string(ValueBin, N), Rest}.