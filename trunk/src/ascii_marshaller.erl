%% Author: carl
%% Created: 12 Feb 2011
%% Description: TODO: Add description to ascii_marshaller
-module(ascii_marshaller).

%%
%% Include files
%%


%%
%% Exported Functions
%%
-export([marshall/1, marshall/2]).

%%
%% API Functions
%%
marshall(Msg) ->
	marshall(Msg, iso8583_fields).

marshall(Msg, EncodingRules) ->
	Mti = iso8583_message:get(0, Msg),
	[0|Fields] = iso8583_message:get_fields(Msg),
	Mti ++ bitmap(Fields) ++ encode(Fields, Msg, EncodingRules).
	
%%
%% Local Functions
%%
bitmap([]) ->
	[];
bitmap(Fields) ->
	NumBitMaps = (lists:max(Fields) + 63) div 64,
	ExtensionBits = [Bit * 64 - 127 || Bit <- lists:seq(2, NumBitMaps)],
	BitMap = lists:duplicate(NumBitMaps * 8, 0),
	string_utils:string_to_ascii_hex(bitmap(lists:sort(ExtensionBits ++ Fields), BitMap)).

bitmap([], Result) ->
	Result;
bitmap([Field|Tail], Result) when Field > 0 ->
	ByteNum = (Field - 1) div 8,
	BitNum = 7 - ((Field - 1) rem 8),
	{Left, Right} = lists:split(ByteNum, Result),
	[ToUpdate | RightRest] = Right,
	bitmap(Tail, Left ++ ([ToUpdate + (1 bsl BitNum)]) ++ RightRest).

encode(Fields, Msg, EncodingRules) ->
	lists:reverse(encode(Fields, Msg, [], EncodingRules)).

encode([], _Msg, Result, _EncodingRules) ->
	Result;
encode([Field|Tail], Msg, Result, EncodingRules) ->
	Encoding = EncodingRules:get_encoding(Field),
	Value = iso8583_message:get(Field, Msg),
	EncodedValue = encode_field(Encoding, Value),
	encode(Tail, Msg, lists:reverse(EncodedValue) ++ Result, EncodingRules).

encode_field({n, llvar, Length}, Value) when length(Value) =< Length ->
	string_utils:integer_to_string(length(Value), 2) ++ Value;
encode_field({n, lllvar, Length}, Value) when length(Value) =< Length ->
	string_utils:integer_to_string(length(Value), 3) ++ Value;
encode_field({an, llvar, Length}, Value) when length(Value) =< Length ->
	string_utils:integer_to_string(length(Value), 2) ++ Value;
encode_field({an, lllvar, Length}, Value) when length(Value) =< Length ->
	string_utils:integer_to_string(length(Value), 3) ++ Value;
encode_field({ans, llvar, Length}, Value) when length(Value) =< Length ->
	string_utils:integer_to_string(length(Value), 2) ++ Value;
encode_field({ans, lllvar, Length}, Value) when length(Value) =< Length ->
	string_utils:integer_to_string(length(Value), 3) ++ Value;
encode_field({n, fixed, Length}, Value) when length(Value) =< Length ->
	IntValue = list_to_integer(Value),
	string_utils:integer_to_string(IntValue, Length);
encode_field({an, fixed, Length}, Value) when length(Value) =< Length ->
	string_utils:pad_with_trailing_spaces(Value, Length);
encode_field({ans, fixed, Length}, Value) when length(Value) =< Length ->
	string_utils:pad_with_trailing_spaces(Value, Length);
encode_field({x_n, fixed, Length}, [Head | Value]) when [Head] =:= "C" orelse [Head] =:= "D" ->
	IntValue = list_to_integer(Value),
	[Head] ++ string_utils:integer_to_string(IntValue, Length);
encode_field({z, llvar, Length}, Value) when length(Value) =< Length ->
	string_utils:integer_to_string(length(Value), 2) ++ Value;
encode_field({b, Length}, Value) when size(Value) =:= Length ->
	string_utils:binary_to_ascii_hex(Value).
	
