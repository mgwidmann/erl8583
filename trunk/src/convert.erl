% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

%% @author CA Meijer
%% @copyright 2011 CA Meijer
%% @doc convert. This module provides a number of functions for converting
%%      between representations of data.
%% @end

-module(convert).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([string_to_ascii_hex/1, 
		 ascii_hex_to_string/1, 
		 integer_to_string/2, 
		 pad_with_trailing_spaces/2,
		 binary_to_ascii_hex/1,
		 ascii_hex_to_binary/1,
		 concat_binaries/1,
		 concat_binaries/2,
		 integer_to_bcd/2,
		 ascii_hex_to_bcd/2,
		 bcd_to_integer/1,
		 bcd_to_ascii_hex/3,
		 track2_to_string/2,
		 string_to_track2/1]).

%%
%% API Functions
%%

%% @doc Converts a string() of characters to a list containing
%%      the ASCII hex equivalents.
-spec(string_to_ascii_hex(string()) -> string()).

string_to_ascii_hex(Str) ->
	string_to_ascii_hex(Str, []).

%% @doc Converts a string() containing ASCII hex characters
%%      to an equivalent string().
-spec(ascii_hex_to_string(string()) -> string()).

ascii_hex_to_string(HexStr) ->
	ascii_hex_to_string(HexStr, []).

%% @doc Converts an integer to a string of fixed length with
%%      leading zeroes if necessary.
-spec(integer_to_string(integer(), integer()) -> string()).

integer_to_string(Value, Length) ->
	pad_with_zeroes(Length, integer_to_list(Value)).

%% @doc Pads a string with a number of spaces so that the
%%      resultant string has specified length.
-spec(pad_with_trailing_spaces(string(), integer()) -> string()).

pad_with_trailing_spaces(List, Length) ->
	lists:reverse(pad_with_leading_spaces(lists:reverse(List), Length)).

%% @doc Returns the ASCII hex encoding of a binary value.
-spec(binary_to_ascii_hex(binary()) -> string()).

binary_to_ascii_hex(Bin) ->
	binary_to_ascii_hex(binary_to_list(Bin), []).

%% @doc Returns the binary value corresponding to an ASCII hex string.
-spec(ascii_hex_to_binary(string()) -> binary()).

ascii_hex_to_binary(List) ->
	Bytes = ascii_hex_to_bytes(List, []),
	list_to_binary(Bytes).
	
concat_binaries(List) ->
	lists:foldr({convert, concat_binaries}, <<>>, List).

concat_binaries(X, Y) ->
	<<X/binary, Y/binary>>.

integer_to_bcd(Value, Length) ->
	integer_to_bcd(Value, Length, []).

ascii_hex_to_bcd(Value, PaddingChar) when length(Value) rem 2 =:= 1 ->
	ascii_hex_to_bcd(Value ++ PaddingChar, PaddingChar);
ascii_hex_to_bcd(Value, _PaddingChar) ->
	ascii_hex_to_bcd2(Value, []).	
	
bcd_to_integer(Bcd) ->
	BcdList = binary_to_list(Bcd),
	F = fun(Value, Acc) ->
				Dig1 = Value div 16,
				Dig2 = Value rem 16,
				100 * Acc + 10 * Dig1 + Dig2
		end,
	lists:foldl(F, 0, BcdList).

bcd_to_ascii_hex(Bcd, Length, PaddingChar) when size(Bcd) =:= (Length + 1) div 2 ->
	IntValue = bcd_to_integer(Bcd),
	case Length rem 2 of
		0 ->
			integer_to_string(IntValue, Length);
		1 ->
			StrippedValue = IntValue - ascii_hex_to_digit(PaddingChar),
			0 = StrippedValue rem 10,
			integer_to_string(StrippedValue div 10, Length)
	end.

track2_to_string(Data, Length) ->
	lists:sublist(track2_to_string2(Data, []), 1, Length).

string_to_track2(Data) ->
	string_to_track2(Data, <<>>, 0, true).

%%
%% Local Functions
%%
digit_to_ascii_hex(D) when D >= 0 andalso D =< 9 ->
	[48+D];
digit_to_ascii_hex(D) when D >= 10 andalso D =< 15 ->
	[55+D].

ascii_hex_to_digit([A]) when A >= $0 andalso A =< $9 ->
	A - $0;
ascii_hex_to_digit([A]) when A >= $A andalso A =< $F ->
	A - 55;
ascii_hex_to_digit([A]) when A >= $a andalso A =< $f ->
	A - 87.

string_to_ascii_hex([], Result) ->
	lists:reverse(Result);
string_to_ascii_hex([Char|Tail], Result) ->
	Msb = Char div 16,
	Lsb = Char rem 16,
	string_to_ascii_hex(Tail, digit_to_ascii_hex(Lsb) ++ 
							digit_to_ascii_hex(Msb) ++
							Result).

ascii_hex_to_string([], Result) ->
	lists:reverse(Result);
ascii_hex_to_string([Dig1, Dig2 | Tail], Result) ->
	Char = [ascii_hex_to_digit([Dig1]) * 16 + ascii_hex_to_digit([Dig2])],
	ascii_hex_to_string(Tail, Char ++ Result).

pad_with_zeroes(Length, Value) when Length =:= length(Value) ->
	Value;
pad_with_zeroes(Length, Value) when Length > length(Value) ->
	pad_with_zeroes(Length, "0" ++ Value).

pad_with_leading_spaces(List, Length) when length(List) =:= Length ->
	List;
pad_with_leading_spaces(List, Length) when length(List) < Length ->
	pad_with_leading_spaces(" " ++ List, Length).

binary_to_ascii_hex([], Result) ->
	lists:reverse(Result);
binary_to_ascii_hex([H|T], Result) ->
	Msn = H div 16,
	Lsn = H rem 16,
	binary_to_ascii_hex(T, digit_to_ascii_hex(Lsn) ++ digit_to_ascii_hex(Msn) ++ Result).

ascii_hex_to_bytes([], Result) ->
	lists:reverse(Result);
ascii_hex_to_bytes([Msd, Lsd | Tail], Result) ->
	Msn = ascii_hex_to_digit([Msd]),
	Lsn = ascii_hex_to_digit([Lsd]),
	ascii_hex_to_bytes(Tail, [Msn * 16 + Lsn] ++ Result).

integer_to_bcd(0, 0, List) ->
	[Head|Tail] = List,
	case length(List) rem 2 of
		0 ->
			Result = concat_adjacent_bytes(List, []),
			list_to_binary(Result);
		1 when Head =< 9 ->
			Result = concat_adjacent_bytes(Tail, []),
			list_to_binary([Head|Result])
	end;	
integer_to_bcd(Value, Length, List) when Length > 0 ->
	integer_to_bcd(Value div 10, Length-1, [Value rem 10|List]).

concat_adjacent_bytes([], Result) ->
	lists:reverse(Result);
concat_adjacent_bytes([Dig1, Dig2|Tail], Result) ->
	concat_adjacent_bytes(Tail, [Dig1 * 16 + Dig2|Result]).

ascii_hex_to_bcd2([], Result) ->
	list_to_binary(lists:reverse(Result));
ascii_hex_to_bcd2([Dig1, Dig2|Tail], Result) ->
	Byte = ascii_hex_to_digit([Dig1]) * 16 + ascii_hex_to_digit([Dig2]),
	ascii_hex_to_bcd2(Tail, [Byte|Result]).

track2_to_string2(<<>>, Result) ->
	lists:reverse(Result);
track2_to_string2(Data, Result) ->
	{<<X>>, Rest} = erlang:split_binary(Data, 1),
	track2_to_string2(Rest, [X rem 16 + $0, X div 16 + $0 | Result]).

string_to_track2([], Result, 0, true) ->
	Result;
string_to_track2([], Result, X, false) ->
	concat_binaries(Result, << (X*16) >>);
string_to_track2([H|T], Result, 0, true) ->
	string_to_track2(T, Result, H-$0, false);
string_to_track2([H|T], Result, X, false) ->
	Xupdated = X*16 + H - $0,
	string_to_track2(T, concat_binaries(Result, <<Xupdated>>), 0, true).

	
%%
%% Tests
%%
digit_to_ascii_hex_test() ->
	"0" = digit_to_ascii_hex(0),
	"9" = digit_to_ascii_hex(9),
	"A" = digit_to_ascii_hex(10),
	"F" = digit_to_ascii_hex(15),
	?assertError(_, digit_to_ascii_hex(16)).

ascii_hex_to_digit_test() ->
	0 = ascii_hex_to_digit("0"),
	9 = ascii_hex_to_digit("9"),
	10 = ascii_hex_to_digit("A"),
	15 = ascii_hex_to_digit("F"),
	10 = ascii_hex_to_digit("a"),
	15 = ascii_hex_to_digit("f"),
	?assertError(_, ascii_hex_to_digit("G")).

string_to_ascii_hex_test() ->
	"" = string_to_ascii_hex(""),
	"30" = string_to_ascii_hex("0"),
	"48656C6C6F" = string_to_ascii_hex("Hello").

ascii_hex_to_string_test() ->
	"" = ascii_hex_to_string(""),
	"Hello" = ascii_hex_to_string("48656C6c6F").

binary_to_ascii_hex_test() ->
	"00FFA5" = binary_to_ascii_hex(<<0, 255, 165>>).

ascii_hex_to_binary_test() ->
	<<0, 255, 165>> = ascii_hex_to_binary("00FFa5").

concat_binaries_test() ->
	<<1, 2, 3, 4>> = concat_binaries([<<1>>, <<2, 3>>, <<4>>]).

integer_to_bcd_test() ->
	<<1>> = integer_to_bcd(1, 1),
	<<1>> = integer_to_bcd(1, 2),
	<<0, 1>> = integer_to_bcd(1, 3),
	<<16>> = integer_to_bcd(10, 2),
	<<0, 16>> = integer_to_bcd(10, 3),
	<<9, 153>> = integer_to_bcd(999, 3),
	<<0,18,52,86,120>> = integer_to_bcd(12345678, 10),
	<<1,35,69,103,137>> = integer_to_bcd(123456789, 9),
	?assertError(_, integer_to_bcd(1000, 3)).

ascii_hex_to_bcd_test() ->
	<<48, 31>> = ascii_hex_to_bcd("301", "F"),
	<<64, 31>> = ascii_hex_to_bcd("401", "F"),
	<<48, 16>> = ascii_hex_to_bcd("301", "0"),
	<<18, 52>> = ascii_hex_to_bcd("1234", "0").

bcd_to_integer_test() ->
	17 = bcd_to_integer(<<23>>),
	1 = bcd_to_integer(<<1>>),
	1 = bcd_to_integer(<<0, 1>>),
	123 = bcd_to_integer(<<1, 35>>),
	123456 = bcd_to_integer(<<18, 52, 86>>).

bcd_to_ascii_hex_test() ->
	"1234" = bcd_to_ascii_hex(<<18, 52>>, 4, "F"),
	"123" = bcd_to_ascii_hex(<<18, 63>>, 3, "F"),
	"123" = bcd_to_ascii_hex(<<18, 48>>, 3, "0"),
	"401" = bcd_to_ascii_hex(<<64, 31>>, 3, "F").

track2_to_string_test() ->
	";1234123412341234=0305101193010877?" = track2_to_string(<<177, 35, 65, 35, 65, 35, 65, 35, 77, 3, 5, 16, 17, 147, 1, 8, 119, 240>>,  35).

string_to_track2_test() ->
	<<177, 35, 65, 35, 65, 35, 65, 35, 77, 3, 5, 16, 17, 147, 1, 8, 119, 240>> = convert:string_to_track2(";1234123412341234=0305101193010877?").
