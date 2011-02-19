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

unmarshall(Msg, EncodingRules) ->
	IsoMsg1 = iso8583_message:new(),
	{Mti, Rest} = lists:split(4, Msg),
	IsoMsg2 = iso8583_message:set(0, Mti, IsoMsg1),
	{FieldIds, Fields} = extract_fields(Rest),
	decode_fields(FieldIds, Fields, IsoMsg2, EncodingRules).


%%
%% Local Functions
%%
extract_fields([]) ->
	{[], []};
extract_fields(Message) ->
	BitMapLength = get_bit_map_length(Message),
	{AsciiBitMap, Fields} = lists:split(BitMapLength, Message),
	BitMap = string_utils:ascii_hex_to_string(AsciiBitMap),
	extract_fields(BitMap, 0, 8, {[], Fields}).

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
	{N, Rest} = lists:split(2, Fields),
	lists:split(list_to_integer(N), Rest);
decode_field({n, lllvar, _MaxLength}, Fields) ->
	{N, Rest} = lists:split(3, Fields),
	lists:split(list_to_integer(N), Rest);
decode_field({an, llvar, _MaxLength}, Fields) ->
	{N, Rest} = lists:split(2, Fields),
	lists:split(list_to_integer(N), Rest);
decode_field({an, lllvar, _MaxLength}, Fields) ->
	{N, Rest} = lists:split(3, Fields),
	lists:split(list_to_integer(N), Rest);
decode_field({ans, llvar, _MaxLength}, Fields) ->
	{N, Rest} = lists:split(2, Fields),
	lists:split(list_to_integer(N), Rest);
decode_field({ans, lllvar, _MaxLength}, Fields) ->
	{N, Rest} = lists:split(3, Fields),
	lists:split(list_to_integer(N), Rest);
decode_field({n, fixed, Length}, Fields) ->
	lists:split(Length, Fields);
decode_field({an, fixed, Length}, Fields) ->
	lists:split(Length, Fields);
decode_field({ans, fixed, Length}, Fields) ->
	lists:split(Length, Fields);
decode_field({x_n, fixed, Length}, [Head|Tail]) when [Head] =:= "C" orelse [Head] =:= "D" ->
	lists:split(Length+1, [Head|Tail]);
decode_field({z, llvar, _MaxLength}, Fields) ->
	{N, Rest} = lists:split(2, Fields),
	lists:split(list_to_integer(N), Rest);
decode_field({b, Length}, Fields) ->
	{ValueStr, Rest} = lists:split(2 * Length, Fields),
	Value = string_utils:ascii_hex_to_binary(ValueStr),
	{Value, Rest}.

get_bit_map_length(Msg) ->
	get_bit_map_length(Msg, 16).

get_bit_map_length(Msg, Length) ->
	[HexDig1, HexDig2|_Tail] = Msg,
	<<Byte>> = string_utils:ascii_hex_to_binary([HexDig1, HexDig2]),
	case (Byte band 128) of
		0 ->
			Length;
		_ ->
			{_Msg1, Msg2} = lists:split(16, Msg),
			get_bit_map_length(Msg2, Length+16)
	end.

	