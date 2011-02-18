%% Author: carl
%% Created: 13 Feb 2011
%% Description: TODO: Add description to test_ascii_unmarshaller
-module(test_ascii_unmarshaller).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("field_defines.hrl").

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
unmarshall_mti_test() ->
	Msg = ascii_unmarshaller:unmarshall("0210"),
	"0210" = iso8583_message:get(0, Msg),
	[0] = iso8583_message:get_fields(Msg).

pan_test() ->
	Msg = ascii_unmarshaller:unmarshall("02004000000000000000165234567890123456"),
	"0200" = iso8583_message:get(0, Msg),
	[0, 2] = iso8583_message:get_fields(Msg),
	"5234567890123456" = iso8583_message:get(2, Msg).
	
field_8_9_10_test() ->
	Msg = ascii_unmarshaller:unmarshall("130001C0000000000000000000010000000200000003"),
	"1300" = iso8583_message:get(0, Msg),
	[0, 8, 9, 10] = iso8583_message:get_fields(Msg),
	"00000001" = iso8583_message:get(8, Msg),
	"00000002" = iso8583_message:get(9, Msg),
	"00000003" = iso8583_message:get(10, Msg).

fields_11_12_13_14_test() ->
	Msg = ascii_unmarshaller:unmarshall("0200003C00000000000000123415075520121206"),
	[0, 11, 12, 13, 14] = iso8583_message:get_fields(Msg),
	"150755" = iso8583_message:get(12, Msg),
	"001234" = iso8583_message:get(11, Msg).

fields_15_to_20_test() ->
	Msg = ascii_unmarshaller:unmarshall("02000003F000000000000001000200030004005006"),
	[0, 15, 16, 17, 18, 19, 20] = iso8583_message:get_fields(Msg),
	"0001" = iso8583_message:get(15, Msg),
	"0003" = iso8583_message:get(17, Msg),
	"0004" = iso8583_message:get(18, Msg),
	"006" = iso8583_message:get(20, Msg).

field_26_test() ->
	Msg = ascii_unmarshaller:unmarshall("0200000008400000000000106"),
	[0, 21, 26] = iso8583_message:get_fields(Msg),
	"06" = iso8583_message:get(?POS_CAPTURE_CODE, Msg).

field_27_to_33_test() ->
	Msg = ascii_unmarshaller:unmarshall("02000000003F800000009C00000011D00000022C00000033C00000044035551112345678901"),
	"9" = iso8583_message:get(?AUTHORIZING_ID_RESP_LEN, Msg),
	"C00000011" = iso8583_message:get(?AMOUNT_TRAN_FEE, Msg).

fields_34_35_test() ->
	Msg = ascii_unmarshaller:unmarshall("02000000000060000000261234123412341234123456789035;1234123412341234=0305101193010877?"),
	[0, 34, 35] = iso8583_message:get_fields(Msg),
	"12341234123412341234567890" = iso8583_message:get(?PAN_EXTENDED, Msg),
	";1234123412341234=0305101193010877?" = iso8583_message:get(?TRACK_2_DATA, Msg).

fields_36_37_38_test() ->
	Msg = ascii_unmarshaller:unmarshall("0200000000001C0000000101234567890Query123456 123   "),
	[0, 36, 37, 38] = iso8583_message:get_fields(Msg),
	"1234567890" = iso8583_message:get(?TRACK_3_DATA, Msg),
	"Query123456 " = iso8583_message:get(37, Msg),
	"123   " = iso8583_message:get(38, Msg).

field_39_test() ->
	Msg = ascii_unmarshaller:unmarshall("020000000000020000007 "),
	[0, 39] = iso8583_message:get_fields(Msg),
	"7 " = iso8583_message:get(?RESP_CODE, Msg).

field_40_test() ->
	Msg = ascii_unmarshaller:unmarshall("02000000000001000000R 1"),
	"R 1" = iso8583_message:get(40, Msg).

field_41_test() ->
	Msg = ascii_unmarshaller:unmarshall("02000000000000800000Term#1  "),
	"Term#1  " = iso8583_message:get(?CARD_ACCEPTOR_TERMINAL_ID, Msg).

field_42_test() ->
	Msg = ascii_unmarshaller:unmarshall("02000000000000400000CA ID 123      "),
	"CA ID 123      " = iso8583_message:get(42, Msg).

field_43_test() ->
	Msg = ascii_unmarshaller:unmarshall("02000000000000200000NAME                                  ZA"),
	"NAME                                  ZA" = iso8583_message:get(?CARD_ACCEPTOR_NAME_LOCATION, Msg).
	
field_44_test() ->
	Msg = ascii_unmarshaller:unmarshall("0200000000000010000006Foo123"),
	"Foo123" = iso8583_message:get(?ADDITIONAL_RESP_DATA, Msg).

field_45_test() ->
	Msg = ascii_unmarshaller:unmarshall("0200000000000008000006Foo123"),
	"Foo123" = iso8583_message:get(45, Msg).

field_46_test() ->
	Msg = ascii_unmarshaller:unmarshall("02004000000000040000195234567890123456789013Hello, world!"),
	[0, 2, 46] = iso8583_message:get_fields(Msg),
	"Hello, world!" = iso8583_message:get(46, Msg).

fields_47_48_test() ->
	Msg = ascii_unmarshaller:unmarshall("02000000000000030000006Hello!008Goodbye!"),
	[0, 47, 48] = iso8583_message:get_fields(Msg),
	"Hello!" = iso8583_message:get(?ADDITIONAL_DATA_NATIONAL, Msg),
	"Goodbye!" = iso8583_message:get(?ADDITIONAL_DATA_PRIVATE, Msg).

fields_49_50_51_test() ->
	Msg = ascii_unmarshaller:unmarshall("0200000000000000E000A  B  C  "),
	[0, 49, 50, 51] = iso8583_message:get_fields(Msg),
	"A  " = iso8583_message:get(49, Msg),
	"B  " = iso8583_message:get(50, Msg),
	"C  " = iso8583_message:get(51, Msg).

field_52_test() ->
	Msg = ascii_unmarshaller:unmarshall("02000000000000001000FD0001020304057F"),
	[0, 52] = iso8583_message:get_fields(Msg),
	<<253, 0, 1, 2, 3, 4, 5, 127>> = iso8583_message:get(52, Msg).
