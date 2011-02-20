%% Author: carl
%% Created: 12 Feb 2011
%% Description: TODO: Add description to string_utils
-module(string_utils).

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
		 concat_binaries/2]).

%%
%% API Functions
%%
string_to_ascii_hex(Str) ->
	string_to_ascii_hex(Str, []).

ascii_hex_to_string(HexStr) ->
	ascii_hex_to_string(HexStr, []).

integer_to_string(Value, Length) ->
	pad_with_zeroes(Length, integer_to_list(Value)).

pad_with_trailing_spaces(List, Length) ->
	lists:reverse(pad_with_leading_spaces(lists:reverse(List), Length)).

binary_to_ascii_hex(Bin) ->
	binary_to_ascii_hex(binary_to_list(Bin), []).

ascii_hex_to_binary(List) ->
	Bytes = ascii_hex_to_bytes(List, []),
	list_to_binary(Bytes).
	
concat_binaries(List) ->
	lists:foldr({string_utils, concat_binaries}, <<>>, List).

concat_binaries(X, Y) ->
	<<X/binary, Y/binary>>.

%%
%% Local Functions
%%
digit_to_ascii_hex(D) when D >= 0 andalso D =< 9 ->
	[48+D];
digit_to_ascii_hex(D) when D >= 10 andalso D =< 15 ->
	[55+D].

ascii_hex_to_digit([A]) when A >= 48 andalso A =< 57 ->
	A - 48;
ascii_hex_to_digit([A]) when A >= 65 andalso A =< 70 ->
	A - 55;
ascii_hex_to_digit([A]) when A >= 97 andalso A =< 102 ->
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
