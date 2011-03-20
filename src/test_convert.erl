%% Author: carl
%% Created: 20 Mar 2011
%% Description: TODO: Add description to test_convert
-module(test_convert).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%



%%
%% Local Functions
%%
digit_to_ascii_hex_test() ->
	"0" = convert:digit_to_ascii_hex(0),
	"9" = convert:digit_to_ascii_hex(9),
	"A" = convert:digit_to_ascii_hex(10),
	"F" = convert:digit_to_ascii_hex(15),
	?assertError(_, convert:digit_to_ascii_hex(16)).

ascii_hex_to_digit_test() ->
	0 = convert:ascii_hex_to_digit("0"),
	9 = convert:ascii_hex_to_digit("9"),
	10 = convert:ascii_hex_to_digit("A"),
	15 = convert:ascii_hex_to_digit("F"),
	10 = convert:ascii_hex_to_digit("a"),
	15 = convert:ascii_hex_to_digit("f"),
	?assertError(_, convert:ascii_hex_to_digit("G")).

string_to_ascii_hex_test() ->
	"" = convert:string_to_ascii_hex(""),
	"30" = convert:string_to_ascii_hex("0"),
	"48656C6C6F" = convert:string_to_ascii_hex("Hello").

ascii_hex_to_string_test() ->
	"" = convert:ascii_hex_to_string(""),
	"Hello" = convert:ascii_hex_to_string("48656C6c6F").

binary_to_ascii_hex_test() ->
	"00FFA5" = convert:binary_to_ascii_hex(<<0, 255, 165>>).

ascii_hex_to_binary_test() ->
	<<0, 255, 165>> = convert:ascii_hex_to_binary("00FFa5").

concat_binaries_test() ->
	<<1, 2, 3, 4>> = convert:concat_binaries([<<1>>, <<2, 3>>, <<4>>]).

integer_to_bcd_test() ->
	<<1>> = convert:integer_to_bcd(1, 1),
	<<1>> = convert:integer_to_bcd(1, 2),
	<<0, 1>> = convert:integer_to_bcd(1, 3),
	<<16>> = convert:integer_to_bcd(10, 2),
	<<0, 16>> = convert:integer_to_bcd(10, 3),
	<<9, 153>> = convert:integer_to_bcd(999, 3),
	<<0,18,52,86,120>> = convert:integer_to_bcd(12345678, 10),
	<<1,35,69,103,137>> = convert:integer_to_bcd(123456789, 9),
	?assertError(_, convert:integer_to_bcd(1000, 3)).

ascii_hex_to_bcd_test() ->
	<<48, 31>> = convert:ascii_hex_to_bcd("301", "F"),
	<<64, 31>> = convert:ascii_hex_to_bcd("401", "F"),
	<<48, 16>> = convert:ascii_hex_to_bcd("301", "0"),
	<<18, 52>> = convert:ascii_hex_to_bcd("1234", "0").

bcd_to_integer_test() ->
	17 = convert:bcd_to_integer(<<23>>),
	1 = convert:bcd_to_integer(<<1>>),
	1 = convert:bcd_to_integer(<<0, 1>>),
	123 = convert:bcd_to_integer(<<1, 35>>),
	123456 = convert:bcd_to_integer(<<18, 52, 86>>).

bcd_to_ascii_hex_test() ->
	"1234" = convert:bcd_to_ascii_hex(<<18, 52>>, 4, "F"),
	"123" = convert:bcd_to_ascii_hex(<<18, 63>>, 3, "F"),
	"123" = convert:bcd_to_ascii_hex(<<18, 48>>, 3, "0"),
	"401" = convert:bcd_to_ascii_hex(<<64, 31>>, 3, "F").

track2_to_string_test() ->
	";1234123412341234=0305101193010877?" = convert:track2_to_string(<<177, 35, 65, 35, 65, 35, 65, 35, 77, 3, 5, 16, 17, 147, 1, 8, 119, 240>>,  35).

string_to_track2_test() ->
	<<177, 35, 65, 35, 65, 35, 65, 35, 77, 3, 5, 16, 17, 147, 1, 8, 119, 240>> = convert:string_to_track2(";1234123412341234=0305101193010877?").

