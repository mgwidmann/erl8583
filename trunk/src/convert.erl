%% Author: carl
%% Created: 12 Feb 2011
%% Description: TODO: Add description to string_utils
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
		 bcd_to_integer/1]).

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
	lists:foldr({convert, concat_binaries}, <<>>, List).

concat_binaries(X, Y) ->
	<<X/binary, Y/binary>>.

integer_to_bcd(Value, Length) ->
	integer_to_bcd(Value, Length, []).

ascii_hex_to_bcd(Value, PaddingChar) when length(Value) rem 2 =:= 1 ->
	ascii_hex_to_bcd(Value ++ PaddingChar, PaddingChar);
ascii_hex_to_bcd(Value, _PaddingChar) ->
	ascii_hex_to_bcd2(Value, []).	
	
bcd_to_integer(BcdValue) ->
	F = fun(Digit, Acc) when Digit =< 57 ->
				Acc * 10 + ascii_hex_to_digit([Digit])
		end,
	lists:foldl(F, 0, BcdValue).

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
	<<58, 31>> = ascii_hex_to_bcd("3A1", "F"),
	<<58, 16>> = ascii_hex_to_bcd("3A1", "0"),
	<<18, 52>> = ascii_hex_to_bcd("1234", "0").

bcd_to_integer_test() ->
	19 = bcd_to_integer("19"),
	123 = bcd_to_integer("0123"),
	?assertError(_, bcd_to_integer("3A")).
