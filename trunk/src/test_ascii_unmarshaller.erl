%% Author: carl
%% Created: 13 Feb 2011
%% Description: TODO: Add description to test_ascii_unmarshaller
-module(test_ascii_unmarshaller).

-behaviour(encoding_rules).
-behaviour(custom_marshaller).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("field_defines.hrl").

%%
%% Exported Functions
%%
-export([get_encoding/1, marshal/2, unmarshal/2]).

%%
%% API Functions
%%
get_encoding(2) ->
	{n, fixed, 4};
get_encoding(3) ->
	{custom, ?MODULE};
get_encoding(4) ->
	{custom, ?MODULE}.

marshal(3, _Value) ->
	erlang:error("Shouldn't have been invoked.").
unmarshal(3, [$3|Tail]) ->
	{"Field 3", Tail};
unmarshal(4, [$4|Tail]) ->
	{"Field 4", Tail}.


%%
%% Local Functions
%%
unmarshall_mti_test() ->
	Msg = ascii_unmarshaller:unmarshal("0210"),
	"0210" = iso8583_message:get(0, Msg),
	[0] = iso8583_message:get_fields(Msg).

pan_test() ->
	Msg = ascii_unmarshaller:unmarshal("02004000000000000000165234567890123456"),
	"0200" = iso8583_message:get(0, Msg),
	[0, 2] = iso8583_message:get_fields(Msg),
	"5234567890123456" = iso8583_message:get(2, Msg).
	
field_8_9_10_test() ->
	Msg = ascii_unmarshaller:unmarshal("130001C0000000000000000000010000000200000003"),
	"1300" = iso8583_message:get(0, Msg),
	[0, 8, 9, 10] = iso8583_message:get_fields(Msg),
	"00000001" = iso8583_message:get(8, Msg),
	"00000002" = iso8583_message:get(9, Msg),
	"00000003" = iso8583_message:get(10, Msg).

fields_11_12_13_14_test() ->
	Msg = ascii_unmarshaller:unmarshal("0200003C00000000000000123415075520121206"),
	[0, 11, 12, 13, 14] = iso8583_message:get_fields(Msg),
	"150755" = iso8583_message:get(12, Msg),
	"001234" = iso8583_message:get(11, Msg).

fields_15_to_20_test() ->
	Msg = ascii_unmarshaller:unmarshal("02000003F000000000000001000200030004005006"),
	[0, 15, 16, 17, 18, 19, 20] = iso8583_message:get_fields(Msg),
	"0001" = iso8583_message:get(15, Msg),
	"0003" = iso8583_message:get(17, Msg),
	"0004" = iso8583_message:get(18, Msg),
	"006" = iso8583_message:get(20, Msg).

field_26_test() ->
	Msg = ascii_unmarshaller:unmarshal("0200000008400000000000106"),
	[0, 21, 26] = iso8583_message:get_fields(Msg),
	"06" = iso8583_message:get(?POS_CAPTURE_CODE, Msg).

field_27_to_33_test() ->
	Msg = ascii_unmarshaller:unmarshal("02000000003F800000009C00000011D00000022C00000033C00000044035551112345678901"),
	"9" = iso8583_message:get(?AUTHORIZING_ID_RESP_LEN, Msg),
	"C00000011" = iso8583_message:get(?AMOUNT_TRAN_FEE, Msg).

fields_34_35_test() ->
	Msg = ascii_unmarshaller:unmarshal("02000000000060000000261234123412341234123456789035;1234123412341234=0305101193010877?"),
	[0, 34, 35] = iso8583_message:get_fields(Msg),
	"12341234123412341234567890" = iso8583_message:get(?PAN_EXTENDED, Msg),
	";1234123412341234=0305101193010877?" = iso8583_message:get(?TRACK_2_DATA, Msg).

fields_36_37_38_test() ->
	Msg = ascii_unmarshaller:unmarshal("0200000000001C0000000101234567890Query123456 123   "),
	[0, 36, 37, 38] = iso8583_message:get_fields(Msg),
	"1234567890" = iso8583_message:get(?TRACK_3_DATA, Msg),
	"Query123456 " = iso8583_message:get(37, Msg),
	"123   " = iso8583_message:get(38, Msg).

field_39_test() ->
	Msg = ascii_unmarshaller:unmarshal("020000000000020000007 "),
	[0, 39] = iso8583_message:get_fields(Msg),
	"7 " = iso8583_message:get(?RESP_CODE, Msg).

field_40_test() ->
	Msg = ascii_unmarshaller:unmarshal("02000000000001000000R 1"),
	"R 1" = iso8583_message:get(40, Msg).

field_41_test() ->
	Msg = ascii_unmarshaller:unmarshal("02000000000000800000Term#1  "),
	"Term#1  " = iso8583_message:get(?CARD_ACCEPTOR_TERMINAL_ID, Msg).

field_42_test() ->
	Msg = ascii_unmarshaller:unmarshal("02000000000000400000CA ID 123      "),
	"CA ID 123      " = iso8583_message:get(42, Msg).

field_43_test() ->
	Msg = ascii_unmarshaller:unmarshal("02000000000000200000NAME                                  ZA"),
	"NAME                                  ZA" = iso8583_message:get(?CARD_ACCEPTOR_NAME_LOCATION, Msg).
	
field_44_test() ->
	Msg = ascii_unmarshaller:unmarshal("0200000000000010000006Foo123"),
	"Foo123" = iso8583_message:get(?ADDITIONAL_RESP_DATA, Msg).

field_45_test() ->
	Msg = ascii_unmarshaller:unmarshal("0200000000000008000006Foo123"),
	"Foo123" = iso8583_message:get(45, Msg).

field_46_test() ->
	Msg = ascii_unmarshaller:unmarshal("02004000000000040000195234567890123456789013Hello, world!"),
	[0, 2, 46] = iso8583_message:get_fields(Msg),
	"Hello, world!" = iso8583_message:get(46, Msg).

fields_47_48_test() ->
	Msg = ascii_unmarshaller:unmarshal("02000000000000030000006Hello!008Goodbye!"),
	[0, 47, 48] = iso8583_message:get_fields(Msg),
	"Hello!" = iso8583_message:get(?ADDITIONAL_DATA_NATIONAL, Msg),
	"Goodbye!" = iso8583_message:get(?ADDITIONAL_DATA_PRIVATE, Msg).

fields_49_50_51_test() ->
	Msg = ascii_unmarshaller:unmarshal("0200000000000000E000A  B  C  "),
	[0, 49, 50, 51] = iso8583_message:get_fields(Msg),
	"A  " = iso8583_message:get(49, Msg),
	"B  " = iso8583_message:get(50, Msg),
	"C  " = iso8583_message:get(51, Msg).

field_52_test() ->
	Msg = ascii_unmarshaller:unmarshal("02000000000000001000FD0001020304057F"),
	[0, 52] = iso8583_message:get_fields(Msg),
	<<253, 0, 1, 2, 3, 4, 5, 127>> = iso8583_message:get(52, Msg).

field_53_test() ->
	Msg = ascii_unmarshaller:unmarshal("020040000000000008001952345678901234567890000000000000017"),
	[0, 2, 53] = iso8583_message:get_fields(Msg),
	"0000000000000017" = iso8583_message:get(?SECURITY_RELATED_CONTROL_INFO, Msg).

field_54_test() ->
	Msg = ascii_unmarshaller:unmarshal("02004000000000000400195234567890123456789017Additi0nal Am0unt"),
	[0, 2, 54] = iso8583_message:get_fields(Msg),
	"Additi0nal Am0unt" = iso8583_message:get(?ADDITIONAL_AMOUNTS, Msg).

fields_55_56_57_test() ->
	Msg = ascii_unmarshaller:unmarshal("02004000000000000380195234567890123456789002A1003B22004C333"),
	[0, 2, 55, 56, 57] = iso8583_message:get_fields(Msg),
	"A1" = iso8583_message:get(?RESERVED_ISO1, Msg),
	"B22" = iso8583_message:get(?RESERVED_ISO2, Msg),
	"C333" = iso8583_message:get(?RESERVED_NATIONAL1, Msg).

fields_58_to_63_test() ->
	Msg = ascii_unmarshaller:unmarshal("0200400000000000007E195234567890123456789002A1003B22004C333005D4444006E55555007F666666"),
	"A1" = iso8583_message:get(?RESERVED_NATIONAL2, Msg),
	"B22" = iso8583_message:get(?RESERVED_NATIONAL3, Msg),
	"C333" = iso8583_message:get(?RESERVED_PRIVATE1, Msg),
	"D4444" = iso8583_message:get(?RESERVED_PRIVATE2, Msg),
	"E55555" = iso8583_message:get(?RESERVED_PRIVATE3, Msg),
	"F666666" = iso8583_message:get(?RESERVED_PRIVATE4, Msg).
	
field_64_test() ->
	Msg = ascii_unmarshaller:unmarshal("0200000000000000000180FF01020304057F"),
	[0, 64] = iso8583_message:get_fields(Msg),
	<<128, 255, 1, 2, 3, 4, 5, 127>> = iso8583_message:get(?MESSAGE_AUTHENTICATION_CODE, Msg).

field_66_test() ->
	Msg = ascii_unmarshaller:unmarshal("0200800000000000000040000000000000001"),
	[0, 66] = iso8583_message:get_fields(Msg),
	"1" = iso8583_message:get(66, Msg).

fields_67_to_70_test() ->
	Msg = ascii_unmarshaller:unmarshal("020080000000000000003C0000000000000001002003004"),
	[0, 67, 68, 69, 70] = iso8583_message:get_fields(Msg),
	"01" = iso8583_message:get(67, Msg),
	"002" = iso8583_message:get(68, Msg),
	"003" = iso8583_message:get(69, Msg),
	"004" = iso8583_message:get(70, Msg).
	
fields_71_72_test() ->
	Msg = ascii_unmarshaller:unmarshal("02008000000000000000030000000000000000010002"),
	[0, 71, 72] = iso8583_message:get_fields(Msg),
	"0001" = iso8583_message:get(?MESSAGE_NUMBER, Msg),
	"0002" = iso8583_message:get(?MESSAGE_NUMBER_LAST, Msg).

field_73_test() ->
	Msg = ascii_unmarshaller:unmarshal("020080000000000000000080000000000000110219"),
	[0, 73] = iso8583_message:get_fields(Msg),
	"110219" = iso8583_message:get(?DATE_ACTION, Msg).

field_78_to_82_test() ->
	Msg = ascii_unmarshaller:unmarshal("020080000000000000000007C000000000000000000001000000002200000003330000004444000000055555"),
	[0, 78, 79, 80, 81, 82] = iso8583_message:get_fields(Msg),
	"0000000001" = iso8583_message:get(?TRANSFER_NUMBER, Msg),
	"0000000022" = iso8583_message:get(?TRANSFER_NUMBER_REVERSAL, Msg),
	"0000000333" = iso8583_message:get(?INQUIRIES_NUMBER, Msg),
	"0000004444" = iso8583_message:get(?AUTHORIZATIONS_NUMBER, Msg),
	"000000055555" = iso8583_message:get(?CREDITS_PROCESSING_FEE_AMOUNT, Msg).
	
fields_91_to_94_test() ->
	Msg = ascii_unmarshaller:unmarshal("020080000000000000000000003C00000000A2255555DDDDDDD"),
	[0, 91, 92, 93, 94] = iso8583_message:get_fields(Msg),
	"A" = iso8583_message:get(?FILE_UPDATE_CODE, Msg),
	"22" = iso8583_message:get(?FILE_SECURITY_CODE, Msg),
	"55555" = iso8583_message:get(?RESP_INDICATOR, Msg),
	"DDDDDDD" = iso8583_message:get(?SERVICE_INDICATOR, Msg).
	
field_97_test() ->
	Msg = ascii_unmarshaller:unmarshal("020080000000000000000000000080000000C0000000000000001"),
	[0, 97] = iso8583_message:get_fields(Msg),
	"C0000000000000001" = iso8583_message:get(?AMOUNT_NET_SETTLE, Msg).

field_98_test() ->
	Msg = ascii_unmarshaller:unmarshal("020080000000000000000000000040000000Payee                    "),
	[0, ?PAYEE] = iso8583_message:get_fields(Msg),
	"Payee                    " = iso8583_message:get(?PAYEE, Msg).

field_101_test() ->
	Msg = ascii_unmarshaller:unmarshal("02008000000000000000000000000800000017A loong file name"),
	[0, ?FILE_NAME] = iso8583_message:get_fields(Msg),
	"A loong file name" = iso8583_message:get(?FILE_NAME, Msg).

fields_102_103_104_128_test() ->
	Msg = ascii_unmarshaller:unmarshal("02008000000000000000000000000700000104ID 1281234567890123456789012345678009txn desc.0000000000000000"),
	[0, ?ACCOUNT_ID1, ?ACCOUNT_ID2, ?TRAN_DESCRIPTION, ?MESSAGE_AUTHENTICATION_CODE2] = iso8583_message:get_fields(Msg),
	<<0, 0, 0, 0, 0, 0, 0, 0>> = iso8583_message:get(?MESSAGE_AUTHENTICATION_CODE2, Msg).

encoding_rules_test() ->
	Msg = ascii_unmarshaller:unmarshal("020040000000000000000001", ?MODULE),
	"0001" = iso8583_message:get(2, Msg).

custom_marshaller_test() ->
	Msg = ascii_unmarshaller:unmarshal("0200300000000000000034", ?MODULE),
	"Field 3" = iso8583_message:get(3, Msg),
	"Field 4" = iso8583_message:get(4, Msg).
