%% Author: carl
%% Created: 28 Mar 2011
%% Description: TODO: Add description to erl8583_marshaller_binary_bitmap
-module(erl8583_marshaller_binary_bitmap).

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

%% @doc Constructs a binary representation of the
%%      bitmap for an iso8583message().
%%
%% @spec marshal(iso8583message()) -> binary()
-spec(marshal(iso8583message()) -> binary()).

marshal(Message) ->
	[0|Fields] = erl8583_message:get_fields(Message),
	construct_bitmap(Fields).



%%
%% Local Functions
%%
%% @doc Constructs a binary representation of the bitmap for a list of 
%%      field IDs.
%%
%% @spec construct_bitmap(list(integer())) -> binary()
-spec(construct_bitmap(list(integer())) -> binary()).

construct_bitmap([]) ->
	<<>>;
construct_bitmap(FieldIds) ->
	NumBitMaps = (lists:max(FieldIds) + 63) div 64,
	ExtensionBits = [Bit * 64 - 127 || Bit <- lists:seq(2, NumBitMaps)],
	BitMap = lists:duplicate(NumBitMaps * 8, 0),
	construct_bitmap(lists:sort(ExtensionBits ++ FieldIds), BitMap).

construct_bitmap([], Result) ->
	list_to_binary(Result);
construct_bitmap([Field|Tail], Result) when Field > 0 ->
	ByteNum = (Field - 1) div 8,
	BitNum = 7 - ((Field - 1) rem 8),
	{Left, Right} = lists:split(ByteNum, Result),
	[ToUpdate | RightRest] = Right,
	construct_bitmap(Tail, Left ++ ([ToUpdate + (1 bsl BitNum)]) ++ RightRest).


