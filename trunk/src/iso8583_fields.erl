%% Author: carl
%% Created: 12 Feb 2011
%% Description: TODO: Add description to iso8583_fields
-module(iso8583_fields).

%%
%% Include files
%%
-include("field_defines.hrl").

%%
%% Exported Functions
%%
-export([get_encoding/1]).

%%
%% API Functions
%%
get_encoding(?MTI) ->
	{n, fixed, 4};
get_encoding(?PAN) ->
	{n, llvar, 19};
get_encoding(?PROC_CODE) ->
	{n, fixed, 6};
get_encoding(?AMOUNT_TRAN) ->
	{n, fixed, 12};
get_encoding(?AMOUNT_SETTLE) ->
	{n, fixed, 12};
get_encoding(?AMOUNT_CARDHOLDER_BILLING) ->
	{n, fixed, 12};
get_encoding(?TRANSMISSION_DATE_TIME) ->
	{n, fixed, 10};
get_encoding(?AMOUNT_CARDHOLDER_BILLING_FEE) ->
	{n, fixed, 8};
get_encoding(?CONVERSION_RATE_SETTLE) ->
	{n, fixed, 8};
get_encoding(?CONVERSION_RATE_CARDHOLDER_BILLING) ->
	{n, fixed, 8};
get_encoding(?SYSTEMS_TRACE_AUDIT_NUMBER) ->
	{n, fixed, 6};
get_encoding(?TIME_LOCAL_TRAN) ->
	{n, fixed, 6};
get_encoding(?DATE_LOCAL_TRAN) ->
	{n, fixed, 4};
get_encoding(?DATE_EXP) ->
	{n, fixed, 4};
get_encoding(?DATE_SETTLE) ->
	{n, fixed, 4};
get_encoding(?DATE_CONVERSION) ->
	{n, fixed, 4};
get_encoding(?DATE_CAPTURE) ->
	{n, fixed, 4};
get_encoding(?MERCHANT_TYPE) ->
	{n, fixed, 4};
get_encoding(?ACQUIRER_COUNTRY_CODE) ->
	{n, fixed, 3};
get_encoding(?PAN_EXT_COUNTRY_CODE) ->
	{n, fixed, 3};
get_encoding(?FORWARDING_INST_COUNTRY_CODE) ->
	{n, fixed, 3};
get_encoding(?POS_ENTRY_MODE) ->
	{n, fixed, 3};
get_encoding(?APPLICATION_PAN_NUMBER) ->
	{n, fixed, 3};
get_encoding(?FUNCTION_CODE) ->
	{n, fixed, 3};
get_encoding(?POS_CONDITION_CODE) ->
	{n, fixed, 2};
get_encoding(?POS_CAPTURE_CODE) ->
	{n, fixed, 2};
get_encoding(?AUTHORIZING_ID_RESP_LEN) ->
	{n, fixed, 1};
get_encoding(?AMOUNT_TRAN_FEE) ->
	{x_n, fixed, 8};
get_encoding(?AMOUNT_SETTLE_FEE) ->
	{x_n, fixed, 8};
get_encoding(?AMOUNT_TRAN_PROCESSING_FEE) ->
	{x_n, fixed, 8};
get_encoding(?AMOUNT_SETTLE_PROCESSING_FEE) ->
	{x_n, fixed, 8};
get_encoding(?ACQUIRING_INST_ID_CODE) ->
	{n, llvar, 11};
get_encoding(?FORWARDING_INST_ID_CODE) ->
	{n, llvar, 11};
get_encoding(?PAN_EXTENDED) ->
	{n, llvar, 28};
get_encoding(?TRACK_2_DATA) ->
	{z, llvar, 37};
get_encoding(?TRACK_3_DATA) ->
	{n, lllvar, 104};
get_encoding(?RETRIEVAL_REF_NUM) ->
	{an, fixed, 12};
get_encoding(?AUTHORIZATION_ID_RESP) ->
	{an, fixed, 6};
get_encoding(?RESP_CODE) ->
	{an, fixed, 2};
get_encoding(?SERVICE_RESTRICTION_CODE) ->
	{an, fixed, 3};
get_encoding(?CARD_ACCEPTOR_TERMINAL_ID) ->
	{ans, fixed, 8};
get_encoding(?CARD_ACCEPTOR_ID_CODE) ->
	{ans, fixed, 15};
get_encoding(?CARD_ACCEPTOR_NAME_LOCATION) ->
	{ans, fixed, 40};
get_encoding(?ADDITIONAL_RESP_DATA) ->
	{an, llvar, 25};
get_encoding(?TRACK_1_DATA) ->
	{an, llvar, 76};
get_encoding(?ADDITIONAL_DATA_ISO) ->
	{ans, lllvar, 999};
get_encoding(?ADDITIONAL_DATA_NATIONAL) ->
	{ans, lllvar, 999};
get_encoding(?ADDITIONAL_DATA_PRIVATE) ->
	{ans, lllvar, 999};
get_encoding(?CURRENCY_CODE_TRAN) ->
	{an, fixed, 3};
get_encoding(?CURRENCY_CODE_SETTLE) ->
	{an, fixed, 3};
get_encoding(?CURRENCY_CODE_CARDHOLDER_BILLING) ->
	{an, fixed, 3};
get_encoding(?PERSONAL_ID_NUMBER_DATA) ->
	{b, 8};
get_encoding(?SECURITY_RELATED_CONTROL_INFO) ->
	{n, fixed, 16};
get_encoding(?ADDITIONAL_AMOUNTS) ->
	{an, lllvar, 120};
get_encoding(?RESERVED_ISO1) ->
	{ans, lllvar, 999};
get_encoding(?RESERVED_ISO2) ->
	{ans, lllvar, 999};
get_encoding(?RESERVED_NATIONAL1) ->
	{ans, lllvar, 999};
get_encoding(?RESERVED_NATIONAL2) ->
	{ans, lllvar, 999};
get_encoding(?RESERVED_NATIONAL3) ->
	{ans, lllvar, 999};
get_encoding(?RESERVED_PRIVATE1) ->
	{ans, lllvar, 999};
get_encoding(?RESERVED_PRIVATE2) ->
	{ans, lllvar, 999};
get_encoding(?RESERVED_PRIVATE3) ->
	{ans, lllvar, 999};
get_encoding(?RESERVED_PRIVATE4) ->
	{ans, lllvar, 999};
get_encoding(?MESSAGE_AUTHENTICATION_CODE) ->
	{b, 8};
get_encoding(?SETTLE_CODE) ->
	{n, fixed, 1};
get_encoding(?EXTENDED_PAYMENT_CODE) ->
	{n, fixed, 2};
get_encoding(?RECEIVING_INSTITUTION_COUNTRY_CODE) ->
	{n, fixed, 3};
get_encoding(?SETTLE_INSTITUTION_COUNTRY_CODE) ->
	{n, fixed, 3};
get_encoding(?NETWORK_MANAGEMENT_INFORMATION_CODE) ->
	{n, fixed, 3};
get_encoding(?MESSAGE_NUMBER) ->
	{n, fixed, 4};
get_encoding(?MESSAGE_NUMBER_LAST) ->
	{n, fixed, 4};
get_encoding(?DATE_ACTION) ->
	{n, fixed, 6};
get_encoding(?CREDITS_NUMBER) ->
	{n, fixed, 10};
get_encoding(?CREDITS_REVERSAL_NUMBER) ->
	{n, fixed, 10};
get_encoding(?DEBITS_NUMBER) ->
	{n, fixed, 10};
get_encoding(?DEBITS_REVERSAL_NUMBER) ->
	{n, fixed, 10};
get_encoding(?TRANSFER_NUMBER) ->
	{n, fixed, 10};
get_encoding(?TRANSFER_NUMBER_REVERSAL) ->
	{n, fixed, 10};
get_encoding(?INQUIRIES_NUMBER) ->
	{n, fixed, 10};
get_encoding(?AUTHORIZATIONS_NUMBER) ->
	{n, fixed, 10};
get_encoding(?CREDITS_PROCESSING_FEE_AMOUNT) ->
	{n, fixed, 12};
get_encoding(?CREDITS_TRANSACTION_FEE_AMOUNT) ->
	{n, fixed, 12};
get_encoding(?DEBITS_PROCESSING_FEE_AMOUNT) ->
	{n, fixed, 12};
get_encoding(?DEBITS_TRANSACTION_FEE_AMOUNT) ->
	{n, fixed, 12};
get_encoding(?CREDITS_AMOUNT) ->
	{n, fixed, 16};
get_encoding(?CREDITS_REVERSAL_AMOUNT) ->
	{n, fixed, 16};
get_encoding(?DEBITS_AMOUNT) ->
	{n, fixed, 16};
get_encoding(?DEBITS_REVERSAL_AMOUNT) ->
	{n, fixed, 16};
get_encoding(?ORIGINAL_DATA_ELEMENTS) ->
	{n, fixed, 42};
get_encoding(?FILE_UPDATE_CODE) ->
	{an, fixed, 1};
get_encoding(?FILE_SECURITY_CODE) ->
	{an, fixed, 2};
get_encoding(?RESP_INDICATOR) ->
	{an, fixed, 5};
get_encoding(?SERVICE_INDICATOR) ->
	{an, fixed, 7};
get_encoding(?REPLACEMENT_AMOUNTS) ->
	{an, fixed, 42};
get_encoding(?MESSAGE_SECURITY_CODE) ->
	{b, 8};
get_encoding(?AMOUNT_NET_SETTLE) ->
	{x_n, fixed, 16};
get_encoding(?PAYEE) ->
	{ans, fixed, 25};
get_encoding(?SETTLE_INSTITUTION_ID_CODE) ->
	{n, llvar, 11};
get_encoding(?RECEIVING_INSTITUTION_ID_CODE) ->
	{n, llvar, 11};
get_encoding(?FILE_NAME) ->
	{ans, llvar, 17}.


%%
%% Local Functions
%%

