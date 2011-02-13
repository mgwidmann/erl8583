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
	{n, fixed, 3}.

%%
%% Local Functions
%%

