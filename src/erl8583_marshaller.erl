%% Author: carl
%% Created: 02 Apr 2011
%% Description: TODO: Add description to erl8583_marshaller
-module(erl8583_marshaller).

%%
%% Include files
%%

%%
%% Records
%%
-record(marshal_options, {field_marshaller, bitmap_marshaller, encoding_rules=erl8583_fields}).

%%
%% Exported Functions
%%
-export([marshal/2]).

%%
%% API Functions
%%
marshal(Message, Options) ->
	OptionsRecord = parse_options(Options, #marshal_options{}),
	FieldMarshalModule = OptionsRecord#marshal_options.field_marshaller,
	EncodingRules = OptionsRecord#marshal_options.encoding_rules,
	if
		FieldMarshalModule =:= undefined ->
			Marshalled1 = [];
		FieldMarshalModule =/= undefined ->
			Mti = FieldMarshalModule:marshal_field(0, erl8583_message:get(0, Message), EncodingRules),
			Marshalled1 = Mti
	end,
	BitmapMarshalModule = OptionsRecord#marshal_options.bitmap_marshaller,
	[0|Fields] = erl8583_message:get_fields(Message),
	if
		BitmapMarshalModule =:= undefined ->
			Marshalled2 = Marshalled1;
		BitmapMarshalModule =/= undefined ->			
			Bitmap = BitmapMarshalModule:marshal_bitmap(Fields),
			Marshalled2 = Marshalled1 ++ Bitmap
	end,
	if
		FieldMarshalModule =:= undefined ->
			Marshalled3 = Marshalled2;
		FieldMarshalModule =/= undefined ->
			Marshalled3 = Marshalled2 ++ encode(Fields, Message, FieldMarshalModule, EncodingRules) 
	end,
	Marshalled3.


%%
%% Local Functions
%%
parse_options([], OptionsRecord) ->
	OptionsRecord;
parse_options([{field_marshaller, Marshaller}|Tail], OptionsRecord) ->
	parse_options(Tail, OptionsRecord#marshal_options{field_marshaller=Marshaller});
parse_options([{bitmap_marshaller, Marshaller}|Tail], OptionsRecord) ->
	parse_options(Tail, OptionsRecord#marshal_options{bitmap_marshaller=Marshaller});
parse_options([{encoding_rules, Rules}|Tail], OptionsRecord) ->
	parse_options(Tail, OptionsRecord#marshal_options{encoding_rules=Rules}).

encode(Fields, Msg, FieldMarshaller, EncodingRules) ->
	encode(Fields, Msg, [], FieldMarshaller, EncodingRules).

encode([], _Msg, Result, _FieldMarshaller, _EncodingRules) ->
	lists:reverse(Result);
encode([FieldId|Tail], Msg, Result, FieldMarshaller, EncodingRules) ->
	Value = erl8583_message:get(FieldId, Msg),
	EncodedValue = FieldMarshaller:marshal_field(FieldId, Value, EncodingRules),
	encode(Tail, Msg, lists:reverse(EncodedValue) ++ Result, FieldMarshaller, EncodingRules).
