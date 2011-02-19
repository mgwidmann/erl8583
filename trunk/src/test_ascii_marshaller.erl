%% Author: carl
%% Created: 12 Feb 2011
%% Description: TODO: Add description to test_ascii_marshaller
-module(test_ascii_marshaller).

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

%% Test that a message with only an MTI can be exported.
mti_only_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	"0200" ++ X = ascii_marshaller:marshall(Msg2),
	Msg3 = iso8583_message:set(0, "0210", Msg1),
	"0210" ++ X = ascii_marshaller:marshall(Msg3).

pan_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(2, "5234567890123456", Msg2),	
	"02004000000000000000165234567890123456" = ascii_marshaller:marshall(Msg3).

proc_code_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0100", Msg1),
	Msg3 = iso8583_message:set(3, "01234", Msg2),	
	"01002000000000000000001234" = ascii_marshaller:marshall(Msg3).

amount_tran_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(4, "30000", Msg2),	
	"02001000000000000000000000030000" = ascii_marshaller:marshall(Msg3).
	
amount_settle_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(5, "1", Msg2),	
	"02000800000000000000000000000001" = ascii_marshaller:marshall(Msg3).

fields_5_6_7_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(5, "1", Msg2),	
	Msg4 = iso8583_message:set(6, "2", Msg3),	
	Msg5 = iso8583_message:set(7, "0131081200", Msg4),	
	"02000E000000000000000000000000010000000000020131081200" = ascii_marshaller:marshall(Msg5).

fields_8_9_10_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "1300", Msg1),
	Msg3 = iso8583_message:set(8, "1", Msg2),	
	Msg4 = iso8583_message:set(9, "2", Msg3),	
	Msg5 = iso8583_message:set(10, "3", Msg4),	
	"130001C0000000000000000000010000000200000003" = ascii_marshaller:marshall(Msg5).

fields_11_12_13_14_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(11, "1234", Msg2),	
	Msg4 = iso8583_message:set(12, "150755", Msg3),	
	Msg5 = iso8583_message:set(13, "2012", Msg4),
	Msg6 = iso8583_message:set(14, "1206", Msg5),
	"0200003C00000000000000123415075520121206" = ascii_marshaller:marshall(Msg6).

fields_21_to_25_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(?FORWARDING_INST_COUNTRY_CODE, "1", Msg2),	
	Msg4 = iso8583_message:set(?POS_ENTRY_MODE, "2", Msg3),	
	Msg5 = iso8583_message:set(?APPLICATION_PAN_NUMBER, "3", Msg4),
	Msg6 = iso8583_message:set(?POS_CONDITION_CODE, "5", Msg5),
	Msg7 = iso8583_message:set(?FUNCTION_CODE, "4", Msg6),
	"020000000F800000000000100200300405" = ascii_marshaller:marshall(Msg7).
	
fields_27_to_33_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(27, "9", Msg2),	
	Msg4 = iso8583_message:set(28, "C11", Msg3),
	Msg5 = iso8583_message:set(29, "D22", Msg4),
	Msg6 = iso8583_message:set(30, "C33", Msg5),
	Msg7 = iso8583_message:set(31, "C44", Msg6),
	Msg8 = iso8583_message:set(32, "555", Msg7),
	Msg9 = iso8583_message:set(33, "12345678901", Msg8),
	"02000000003F800000009C00000011D00000022C00000033C00000044035551112345678901"
		= ascii_marshaller:marshall(Msg9).

fields_34_35_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(34, "12341234123412341234567890", Msg2),	
	Msg4 = iso8583_message:set(35, ";1234123412341234=0305101193010877?", Msg3),
	"02000000000060000000261234123412341234123456789035;1234123412341234=0305101193010877?" =
		ascii_marshaller:marshall(Msg4).

fields_36_37_38_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(36, "1234567890", Msg2),	
	Msg4 = iso8583_message:set(37, "Query123456", Msg3),
	Msg5 = iso8583_message:set(38, "123", Msg4),
	"0200000000001C0000000101234567890Query123456 123   " = ascii_marshaller:marshall(Msg5).

field_39_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(39, "R7", Msg2),
	"02000000000002000000R7" = ascii_marshaller:marshall(Msg3).

field_40_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(?SERVICE_RESTRICTION_CODE, "R 1", Msg2),
	"02000000000001000000R 1" = ascii_marshaller:marshall(Msg3).
	
field_41_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(41, "Term#1", Msg2),
	"02000000000000800000Term#1  " = ascii_marshaller:marshall(Msg3).

field_42_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(?CARD_ACCEPTOR_ID_CODE, "CA ID 123", Msg2),
	"02000000000000400000CA ID 123      " = ascii_marshaller:marshall(Msg3).

field_43_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(43, "NAME                                  ZA", Msg2),
	"02000000000000200000NAME                                  ZA" = ascii_marshaller:marshall(Msg3).

field_44_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(44, "Foo123", Msg2),
	"0200000000000010000006Foo123" = ascii_marshaller:marshall(Msg3).
	
field_45_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(?TRACK_1_DATA, "Foo123", Msg2),
	"0200000000000008000006Foo123" = ascii_marshaller:marshall(Msg3).
	
field_46_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(2, "5234567890123456789", Msg2),
	Msg4 = iso8583_message:set(?ADDITIONAL_DATA_ISO, "Hello, world!", Msg3),
	"02004000000000040000195234567890123456789013Hello, world!" = ascii_marshaller:marshall(Msg4).

fields_47_48_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(47, "Hello!", Msg2),
	Msg4 = iso8583_message:set(48, "Goodbye!", Msg3),
	"02000000000000030000006Hello!008Goodbye!" = ascii_marshaller:marshall(Msg4).

fields_49_50_51_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(?CURRENCY_CODE_TRAN, "A", Msg2),
	Msg4 = iso8583_message:set(?CURRENCY_CODE_SETTLE, "B", Msg3),
	Msg5 = iso8583_message:set(?CURRENCY_CODE_CARDHOLDER_BILLING, "C", Msg4),
	"0200000000000000E000A  B  C  " = ascii_marshaller:marshall(Msg5).

field_52_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(52, <<253, 0, 1, 2, 3, 4, 5, 127>>, Msg2),
	"02000000000000001000FD0001020304057F" = ascii_marshaller:marshall(Msg3).

field_53_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(2, "5234567890123456789", Msg2),
	Msg4 = iso8583_message:set(?SECURITY_RELATED_CONTROL_INFO, "17", Msg3),
	"020040000000000008001952345678901234567890000000000000017" = ascii_marshaller:marshall(Msg4).
	
field_54_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(2, "5234567890123456789", Msg2),
	Msg4 = iso8583_message:set(?ADDITIONAL_AMOUNTS, "Additi0nal Am0unt", Msg3),
	"02004000000000000400195234567890123456789017Additi0nal Am0unt" = ascii_marshaller:marshall(Msg4).

fields_55_56_57_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(2, "5234567890123456789", Msg2),
	Msg4 = iso8583_message:set(55, "A1", Msg3),
	Msg5 = iso8583_message:set(56, "B22", Msg4),
	Msg6 = iso8583_message:set(57, "C333", Msg5),
	"02004000000000000380195234567890123456789002A1003B22004C333" = ascii_marshaller:marshall(Msg6).
	
field_64_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(64, <<128, 255, 1, 2, 3, 4, 5, 127>>, Msg2),
	"0200000000000000000180FF01020304057F" = ascii_marshaller:marshall(Msg3).

field_66_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(?SETTLE_CODE, "9", Msg2),
	"0200800000000000000040000000000000009" = ascii_marshaller:marshall(Msg3),
	Msg3 = ascii_unmarshaller:unmarshall(ascii_marshaller:marshall(Msg3)).
	
fields_67_to_70_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(?EXTENDED_PAYMENT_CODE, "1", Msg2),
	Msg4 = iso8583_message:set(?RECEIVING_INSTITUTION_COUNTRY_CODE, "2", Msg3),
	Msg5 = iso8583_message:set(?SETTLE_INSTITUTION_COUNTRY_CODE, "3", Msg4),
	Msg6 = iso8583_message:set(?NETWORK_MANAGEMENT_INFORMATION_CODE, "4", Msg5),
	"020080000000000000003C0000000000000001002003004" = ascii_marshaller:marshall(Msg6).
	
fields_71_72_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(?MESSAGE_NUMBER, "1", Msg2),
	Msg4 = iso8583_message:set(?MESSAGE_NUMBER_LAST, "2", Msg3),
	"02008000000000000000030000000000000000010002" = ascii_marshaller:marshall(Msg4).

fields_74_to_77_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(?CREDITS_NUMBER, "1", Msg2),
	Msg4 = iso8583_message:set(?CREDITS_REVERSAL_NUMBER, "22", Msg3),
	Msg5 = iso8583_message:set(?DEBITS_NUMBER, "333", Msg4),
	Msg6 = iso8583_message:set(?DEBITS_REVERSAL_NUMBER, "4444", Msg5),
	"0200800000000000000000780000000000000000000001000000002200000003330000004444" = ascii_marshaller:marshall(Msg6).

fields_83_to_90_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(?CREDITS_TRANSACTION_FEE_AMOUNT, "1", Msg2),
	Msg4 = iso8583_message:set(?DEBITS_PROCESSING_FEE_AMOUNT, "22", Msg3),
	Msg5 = iso8583_message:set(?DEBITS_TRANSACTION_FEE_AMOUNT, "333", Msg4),
	Msg6 = iso8583_message:set(?CREDITS_AMOUNT, "4444", Msg5),
	Msg7 = iso8583_message:set(?CREDITS_REVERSAL_AMOUNT, "55555", Msg6),
	Msg8 = iso8583_message:set(?DEBITS_AMOUNT, "666666", Msg7),
	Msg9 = iso8583_message:set(?DEBITS_REVERSAL_AMOUNT, "7777777", Msg8),
	Msg10 = iso8583_message:set(?ORIGINAL_DATA_ELEMENTS, "88888888", Msg9),
	"0200800000000000000000003FC0000000000000000000010000000000220000000003330000000000004444000000000005555500000000006666660000000007777777000000000000000000000000000000000088888888" = ascii_marshaller:marshall(Msg10).

fields_95_96_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(?REPLACEMENT_AMOUNTS, "1", Msg2),
	Msg4 = iso8583_message:set(?MESSAGE_SECURITY_CODE, <<1,0,0,0,0,0,0,255>>, Msg3),
	"0200800000000000000000000003000000001                                         01000000000000FF" = ascii_marshaller:marshall(Msg4).
	
fields_99_100_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(?SETTLE_INSTITUTION_ID_CODE, "11", Msg2),
	Msg4 = iso8583_message:set(?RECEIVING_INSTITUTION_ID_CODE, "2222", Msg3),
	"0200800000000000000000000000300000000211042222" = ascii_marshaller:marshall(Msg4).
	
field_101_test() ->
	Msg1 = iso8583_message:new(),
	Msg2 = iso8583_message:set(0, "0200", Msg1),
	Msg3 = iso8583_message:set(?FILE_NAME, "A loong file name", Msg2),
	"02008000000000000000000000000800000017A loong file name" = ascii_marshaller:marshall(Msg3).
	
%%
%% Local Functions
%%

