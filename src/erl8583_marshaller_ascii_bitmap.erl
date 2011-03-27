%% Author: carl
%% Created: 27 Mar 2011
%% Description: TODO: Add description to erl8583_marshaller_ascii_bitmap
-module(erl8583_marshaller_ascii_bitmap).

%%
%% Include files
%%
%% @headerfile "../include/erl8583_types.hrl"
-include("erl8583_types.hrl").

%%
%% Exported Functions
%%
-export([marshal/1]).

%%
%% API Functions
%%
%% @doc Constructs an ASCII string representation of the
%%      bitmap for an iso8583message().
%%
%% @spec marshal(iso8583message()) -> string()
-spec(marshal(iso8583message()) -> string()).

marshal(Message) ->
	[0|Fields] = erl8583_message:get_fields(Message),
	construct_bitmap(Fields).

%%
%% Local Functions
%%
construct_bitmap([]) ->
	[];
construct_bitmap(FieldIds) ->
	NumBitMaps = (lists:max(FieldIds) + 63) div 64,
	ExtensionBits = [Bit * 64 - 127 || Bit <- lists:seq(2, NumBitMaps)],
	BitMap = lists:duplicate(NumBitMaps * 8, 0),
	erl8583_convert:string_to_ascii_hex(construct_bitmap(lists:sort(ExtensionBits ++ FieldIds), BitMap)).

construct_bitmap([], Result) ->
	Result;
construct_bitmap([Field|Tail], Result) when Field > 0 ->
	ByteNum = (Field - 1) div 8,
	BitNum = 7 - ((Field - 1) rem 8),
	{Left, Right} = lists:split(ByteNum, Result),
	[ToUpdate | RightRest] = Right,
	construct_bitmap(Tail, Left ++ ([ToUpdate + (1 bsl BitNum)]) ++ RightRest).
