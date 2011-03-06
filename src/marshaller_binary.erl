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
-export([marshal/1, 
		 marshal/2, 
		 unmarshal/1, 
		 unmarshal/2, 
		 construct_bitmap/1, 
		 extract_fields/1,
		 encode_field/2,
		 encode_data_element/2,
		 decode_field/2,
		 decode_data_element/2]).

%%
%% API Functions
%%
marshal(Msg) ->
	marshal(Msg, ?MODULE).

marshal(Msg, FieldMarshaller) ->
	Mti = iso8583_message:get(0, Msg),
	MtiBits = convert:ascii_hex_to_binary(Mti),
	[0|Fields] = iso8583_message:get_fields(Msg),
	BitMap = construct_bitmap(Fields),
	EncodedFields = encode(Fields, Msg, FieldMarshaller),
	<< MtiBits/binary, BitMap/binary, EncodedFields/binary>>.

unmarshal(Msg) ->
	unmarshal(Msg, ?MODULE).

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

encode_data_element({n, llvar, Length}, Value) when length(Value) =< Length ->
	LField = convert:integer_to_bcd(length(Value), 2),
	VField = convert:ascii_hex_to_bcd(Value, "0"),
	convert:concat_binaries(LField, VField);
encode_data_element({z, llvar, Length}, Value) when length(Value) =< Length ->
	LField = convert:integer_to_bcd(length(Value), 2),
	VField = convert:string_to_track2(Value),
	convert:concat_binaries(LField, VField);
encode_data_element({n, fixed, Length}, Value) ->
	case Length rem 2 of
		0 ->
			PaddedValue = convert:integer_to_string(list_to_integer(Value), Length);
		1 ->
			PaddedValue = convert:integer_to_string(list_to_integer(Value), Length+1)
	end,
	convert:ascii_hex_to_bcd(PaddedValue, "0");
encode_data_element({an, fixed, Length}, Value) ->
	list_to_binary(convert:pad_with_trailing_spaces(Value, Length));
encode_data_element({ans, fixed, Length}, Value) ->
	list_to_binary(convert:pad_with_trailing_spaces(Value, Length));
encode_data_element({an, llvar, Length}, Value) when length(Value) =< Length ->
	LField = convert:integer_to_bcd(length(Value), 2),
	convert:concat_binaries(LField, list_to_binary(Value));
encode_data_element({ns, llvar, Length}, Value) when length(Value) =< Length ->
	LField = convert:integer_to_bcd(length(Value), 2),
	convert:concat_binaries(LField, list_to_binary(Value));
encode_data_element({ans, llvar, Length}, Value) when length(Value) =< Length ->
	LField = convert:integer_to_bcd(length(Value), 2),
	convert:concat_binaries(LField, list_to_binary(Value));
encode_data_element({ans, lllvar, Length}, Value) when length(Value) =< Length ->
	LField = convert:integer_to_bcd(length(Value), 3),
	convert:concat_binaries(LField, list_to_binary(Value));
encode_data_element({x_n, fixed, Length}, [Head | Value]) when Head =:= $C orelse Head =:= $D ->
	IntValue = list_to_integer(Value),
	convert:concat_binaries(<<Head>>,  convert:integer_to_bcd(IntValue, Length));
encode_data_element({b, Length}, Value) when size(Value) =:= Length ->
	Value.

decode_data_element({n, llvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 1),
	N = convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, (N+1) div 2), 
	{convert:bcd_to_ascii_hex(ValueBin, N, "0"), Rest};
decode_data_element({n, lllvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 2),
	N = convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, (N+1) div 2), 
	{convert:bcd_to_ascii_hex(ValueBin, N, "0"), Rest};
decode_data_element({an, llvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 1),
	N = convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, N), 
	{binary_to_list(ValueBin), Rest};
decode_data_element({ns, llvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 1),
	N = convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, N), 
	{binary_to_list(ValueBin), Rest};
decode_data_element({ans, llvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 1),
	N = convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, N), 
	{binary_to_list(ValueBin), Rest};
decode_data_element({ans, lllvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 2),
	N = convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, N), 
	{binary_to_list(ValueBin), Rest};
decode_data_element({n, fixed, Length}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, (Length + 1) div 2),
	case Length rem 2 of
		0 ->
			{convert:bcd_to_ascii_hex(NBin, Length, "0"), RestBin};
		1 ->
			[$0|AsciiHex] = convert:bcd_to_ascii_hex(NBin, Length+1, "0"),
			{AsciiHex, RestBin}
	end;
decode_data_element({an, fixed, Length}, Fields) ->
	{FieldBin, RestBin} = split_binary(Fields, Length),
	FieldStr = binary_to_list(FieldBin),
	{convert:pad_with_trailing_spaces(FieldStr, Length), RestBin};
decode_data_element({ans, fixed, Length}, Fields) ->
	{FieldBin, RestBin} = split_binary(Fields, Length),
	FieldStr = binary_to_list(FieldBin),
	{convert:pad_with_trailing_spaces(FieldStr, Length), RestBin};
decode_data_element({x_n, fixed, Length}, Fields) ->
	{FieldBin, RestBin} = split_binary(Fields, Length div 2 + 1),
	{<<X>>, Value} = split_binary(FieldBin, 1),
	ValueStr = convert:bcd_to_ascii_hex(Value, Length, "0"),
	case X =:= $C orelse X =:= $D of
		true ->
			{[X] ++ ValueStr, RestBin}
	end;
decode_data_element({z, llvar, _MaxLength}, Fields) ->
	{NBin, RestBin} = split_binary(Fields, 1),
	N = convert:bcd_to_integer(NBin),
	{ValueBin, Rest} = split_binary(RestBin, (N+1) div 2), 
	{convert:track2_to_string(ValueBin, N), Rest};
decode_data_element({b, Length}, Fields) ->
	split_binary(Fields, Length).

encode_field(FieldId, Value) ->
	Pattern = iso8583_fields:get_encoding(FieldId),
	encode_data_element(Pattern, Value).

decode_field(FieldId, Fields) ->
	Pattern = iso8583_fields:get_encoding(FieldId),
	decode_data_element(Pattern, Fields).

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
