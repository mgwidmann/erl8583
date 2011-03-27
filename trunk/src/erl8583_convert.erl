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
%% @doc This module provides a number of functions for converting
%%      between representations of data.

-module(erl8583_convert).

%%
%% Include files
%%

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
		 string_to_track2/1,
		 ascii_hex_to_digit/1,
		 digit_to_ascii_hex/1,
		 strip_trailing_spaces/1,
		 strip_leading_zeroes/1,
		 ascii_to_ebcdic/1]).

%%
%% API Functions
%%

%% @doc Converts a string() of characters to a list containing
%%      the ASCII hex equivalents.
%%
%% @spec string_to_ascii_hex(string()) -> string()
-spec(string_to_ascii_hex(string()) -> string()).

string_to_ascii_hex(Str) ->
	string_to_ascii_hex(Str, []).

%% @doc Converts a string() containing ASCII hex characters
%%      to an equivalent string().
%%
%% @spec ascii_hex_to_string(string()) -> string()
-spec(ascii_hex_to_string(string()) -> string()).

ascii_hex_to_string(HexStr) ->
	ascii_hex_to_string(HexStr, []).

%% @doc Converts an integer to a string of fixed length with
%%      leading zeroes if necessary.
%%
%% @spec integer_to_string(integer(), integer()) -> string()
-spec(integer_to_string(integer(), integer()) -> string()).

integer_to_string(Value, Length) ->
	pad_with_zeroes(Length, integer_to_list(Value)).

%% @doc Pads a string with a number of spaces so that the
%%      resultant string has specified length.
%%
%% @spec pad_with_trailing_spaces(string(), integer()) -> string()
-spec(pad_with_trailing_spaces(string(), integer()) -> string()).

pad_with_trailing_spaces(List, Length) ->
	lists:reverse(pad_with_leading_spaces(lists:reverse(List), Length)).

%% @doc Returns the ASCII hex encoding of a binary value.
%%
%% @spec binary_to_ascii_hex(binary()) -> string()
-spec(binary_to_ascii_hex(binary()) -> string()).

binary_to_ascii_hex(Bin) ->
	binary_to_ascii_hex(binary_to_list(Bin), []).

%% @doc Returns the binary value corresponding to an ASCII hex string.
%%
%% @spec ascii_hex_to_binary(string()) -> binary()
-spec(ascii_hex_to_binary(string()) -> binary()).

ascii_hex_to_binary(List) ->
	Bytes = ascii_hex_to_bytes(List, []),
	list_to_binary(Bytes).
	
%% @doc Concatenates a list of binaries and returns the result.
%%
%% @spec concat_binaries(list(binary())) -> binary()
-spec(concat_binaries(list(binary())) -> binary()).

concat_binaries(List) ->
	lists:foldr({erl8583_convert, concat_binaries}, <<>>, List).

%% @doc Concatenates two binaries.
%%
%% @spec concat_binaries(binary(), binary()) -> binary()
-spec(concat_binaries(binary(), binary()) -> binary()).

concat_binaries(X, Y) ->
	<<X/binary, Y/binary>>.

%% @doc Converts an integer to a list of specified length 
%%      of BCD encoded bytes.
%%
%% @spec integer_to_bcd(integer(), integer()) -> list(byte())
-spec(integer_to_bcd(integer(), integer()) -> list(byte())).

integer_to_bcd(Value, Length) ->
	integer_to_bcd(Value, Length, []).

%% @doc Converts an ASCII hex encoded string to a list of BCD
%%      encoded bytes padded with a specified padding character
%%      if the string has odd length.
%%
%% @spec ascii_hex_to_bcd(string(), char()) -> list(byte())
-spec(ascii_hex_to_bcd(string(), char()) -> list(byte())).

ascii_hex_to_bcd(Value, PaddingChar) when length(Value) rem 2 =:= 1 ->
	ascii_hex_to_bcd(Value ++ PaddingChar, PaddingChar);
ascii_hex_to_bcd(Value, _PaddingChar) ->
	ascii_hex_to_bcd2(Value, []).	
	
%% @doc Converts a list of BCD encoded bytes to an integer.
%%
%% @spec bcd_to_integer(list(byte())) -> integer()
-spec(bcd_to_integer(list(byte())) -> integer()).

bcd_to_integer(Bcd) ->
	BcdList = binary_to_list(Bcd),
	F = fun(Value, Acc) ->
				Dig1 = Value div 16,
				Dig2 = Value rem 16,
				100 * Acc + 10 * Dig1 + Dig2
		end,
	lists:foldl(F, 0, BcdList).

%% @doc Converts a BCD encoding of a value of specified length possibly
%%      padded with a padding character to an ASCII hex string.
%%
%% @spec bcd_to_ascii_hex(list(binary()), integer(), char()) -> string()
-spec(bcd_to_ascii_hex(list(binary()), integer(), char()) -> string()).

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

%% @doc Converts a list of track 2 nibbles to a string containing
%%      an ASCII encoding of the same data.
%%
%% @spec track2_to_string(list(byte()), integer()) -> string()
-spec(track2_to_string(list(byte()), integer()) -> string()).

track2_to_string(Data, Length) ->
	lists:sublist(track2_to_string2(Data, []), 1, Length).

%% @doc Converts a string of ASCII characters to a track 2
%%      encoding.
%%
%% @spec string_to_track2(string()) -> list(byte())
-spec(string_to_track2(string()) -> list(byte())).

string_to_track2(Data) ->
	string_to_track2(Data, <<>>, 0, true).

%% @doc Converts a string containing 1 ASCII hex character
%%      to its value.
%%
%% @spec ascii_hex_to_digit(string()) -> integer()
-spec(ascii_hex_to_digit(string()) -> integer()).

ascii_hex_to_digit([A]) when A >= $0 andalso A =< $9 ->
	A - $0;
ascii_hex_to_digit([A]) when A >= $A andalso A =< $F ->
	A - 55;
ascii_hex_to_digit([A]) when A >= $a andalso A =< $f ->
	A - 87.

%% @doc Converts a value in the range 0-15 to a 1 character
%%      string containing the equivalent hexadecimal digit.
%%
%% @spec digit_to_ascii_hex(integer()) -> string()
-spec(digit_to_ascii_hex(integer()) -> string()).

digit_to_ascii_hex(D) when D >= 0 andalso D =< 9 ->
	[48+D];
digit_to_ascii_hex(D) when D >= 10 andalso D =< 15 ->
	[55+D].

%% @doc Strips trailing spaces from a string.
%%
%% @spec strip_trailing_spaces(string()) -> string()
-spec(strip_trailing_spaces(string()) -> string()).

strip_trailing_spaces(Str) ->
	lists:reverse(strip_leading_spaces(lists:reverse(Str))).

%% @doc Strips leading zeroes from a string.
%%
%% @spec strip_leading_zeroes(string()) -> string()
-spec(strip_leading_zeroes(string()) -> string()).

strip_leading_zeroes([$0|Tail]) ->
	strip_leading_zeroes(Tail);
strip_leading_zeroes(Str) ->
	Str.

%% @doc Converts an ASCII string to an EBCDIC binary.
%%
%% @spec ascii_to_ebcdic(string()) -> binary()
-spec(ascii_to_ebcdic(string()) -> binary()).

ascii_to_ebcdic(Str) ->
	ascii_to_ebcdic(Str, []).

%%
%% Local Functions
%%
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

strip_leading_spaces([$ |Tail]) ->
	strip_leading_spaces(Tail);
strip_leading_spaces(Str) ->
	Str.

ascii_to_ebcdic([], Result) ->
	RevResult = lists:reverse(Result),
	erlang:list_to_binary(RevResult);
ascii_to_ebcdic([H|Tail], Result) when H >= $0 andalso H =< $9 ->
	ascii_to_ebcdic(Tail, [H - $0 + 240|Result]);
ascii_to_ebcdic([H|Tail], Result) when H >= $a andalso H =< $i ->
	ascii_to_ebcdic(Tail, [H - $a + 129|Result]);
ascii_to_ebcdic([H|Tail], Result) when H >= $j andalso H =< $r ->
	ascii_to_ebcdic(Tail, [H - $j + 145|Result]);
ascii_to_ebcdic([H|Tail], Result) when H >= $s andalso H =< $z ->
	ascii_to_ebcdic(Tail, [H - $s + 162|Result]);
ascii_to_ebcdic([H|Tail], Result) when H >= $A andalso H =< $I ->
	ascii_to_ebcdic(Tail, [H - $A + 193|Result]);
ascii_to_ebcdic([H|Tail], Result) when H >= $J andalso H =< $R ->
	ascii_to_ebcdic(Tail, [H - $J + 209|Result]);
ascii_to_ebcdic([H|Tail], Result) when H >= $S andalso H =< $Z ->
	ascii_to_ebcdic(Tail, [H - $S + 226|Result]).
