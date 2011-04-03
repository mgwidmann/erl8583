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
-record(marshal_options, {field_marshaller, 
						  bitmap_marshaller, 
						  wrapping_marshaller, 
						  encoding_rules}).

%%
%% Exported Functions
%%
-export([marshal/2, unmarshal/2]).

%%
%% API Functions
%%
marshal(Message, Options) ->
	OptionsRecord = parse_options(Options, #marshal_options{}),
	Marshalled1 = encode_mti(OptionsRecord, Message),
	Marshalled2 = Marshalled1 ++ encode_bitmap(OptionsRecord, Message),
	Marshalled3 = Marshalled2 ++ encode_fields(OptionsRecord, Message),
	wrap_message(OptionsRecord, Message, Marshalled3).

unmarshal(Marshalled, Options) ->
	OptionsRecord = parse_options(Options, #marshal_options{}),
	Marshalled1 = Marshalled,
	{Message, Marshalled2} = decode_mti(OptionsRecord, Marshalled1, erl8583_message:new()),
	{FieldIds, Marshalled3} = decode_bitmap(OptionsRecord, Marshalled2),
	decode_fields(FieldIds, Message, OptionsRecord, Marshalled3).

%%
%% Local Functions
%%
parse_options([], OptionsRecord) ->
	OptionsRecord;
parse_options([{field_marshaller, Marshaller}|Tail], OptionsRecord) ->
	parse_options(Tail, OptionsRecord#marshal_options{field_marshaller=Marshaller});
parse_options([{bitmap_marshaller, Marshaller}|Tail], OptionsRecord) ->
	parse_options(Tail, OptionsRecord#marshal_options{bitmap_marshaller=Marshaller});
parse_options([{wrapping_marshaller, Marshaller}|Tail], OptionsRecord) ->
	parse_options(Tail, OptionsRecord#marshal_options{wrapping_marshaller=Marshaller});
parse_options([{encoding_rules, Rules}|Tail], OptionsRecord) ->
	parse_options(Tail, OptionsRecord#marshal_options{encoding_rules=Rules}).

get_encoding_rules(Options, Message) ->
	if
		Options#marshal_options.encoding_rules =/= undefined ->
			Options#marshal_options.encoding_rules;
		true ->
			Mti = erl8583_message:get(0, Message),
			[Version|_MtiRest] = Mti,
			case Version of
				$0 ->
					erl8583_fields;
				$1 ->
					erl8583_fields_1993;
				$2 ->
					erl8583_fields_2003
			end
	end.

encode_mti(Options, Message) ->
	EncodingRules = get_encoding_rules(Options, Message),
	FieldMarshalModule = Options#marshal_options.field_marshaller,
	if
		FieldMarshalModule =:= undefined ->
			[];
		FieldMarshalModule =/= undefined ->
			FieldMarshalModule:marshal_field(0, erl8583_message:get(0, Message), EncodingRules)
	end.

decode_mti(Options, Marshalled, Message) ->
	Encoding = Options#marshal_options.encoding_rules,
	case Encoding of
		undefined ->
			EncodingRules = erl8583_fields;
		_ ->
			EncodingRules = Encoding
	end,
	FieldMarshalModule = Options#marshal_options.field_marshaller,
	if
		FieldMarshalModule =:= undefined ->
			{Message, Marshalled};
		FieldMarshalModule =/= undefined ->
			{FieldValue, Rest} = FieldMarshalModule:unmarshal_field(0, Marshalled, EncodingRules),
			{erl8583_message:set(0, FieldValue, Message), Rest}
	end.
	
	
encode_bitmap(Options, Message) ->
	BitmapMarshalModule = Options#marshal_options.bitmap_marshaller,
	[0|Fields] = erl8583_message:get_fields(Message),
	if
		BitmapMarshalModule =:= undefined ->
			[];
		BitmapMarshalModule =/= undefined ->			
			BitmapMarshalModule:marshal_bitmap(Fields)
	end.

decode_bitmap(Options, Marshalled) ->
	BitmapMarshalModule = Options#marshal_options.bitmap_marshaller,
	if
		BitmapMarshalModule =:= undefined ->
			{[], Marshalled};
		BitmapMarshalModule =/= undefined ->			
		BitmapMarshalModule:unmarshal_bitmap(Marshalled)
	end.

encode_fields(Options, Message) ->
	[0|Fields] = erl8583_message:get_fields(Message),
	EncodingRules = get_encoding_rules(Options, Message),
	FieldMarshalModule = Options#marshal_options.field_marshaller,
	if
		FieldMarshalModule =:= undefined ->
			[];
		FieldMarshalModule =/= undefined ->
			encode(Fields, Message, FieldMarshalModule, EncodingRules) 
	end.
	
encode(Fields, Msg, FieldMarshaller, EncodingRules) ->
	encode(Fields, Msg, [], FieldMarshaller, EncodingRules).

encode([], _Msg, Result, _FieldMarshaller, _EncodingRules) ->
	lists:reverse(Result);
encode([FieldId|Tail], Msg, Result, FieldMarshaller, EncodingRules) ->
	Value = erl8583_message:get(FieldId, Msg),
	EncodedValue = FieldMarshaller:marshal_field(FieldId, Value, EncodingRules),
	encode(Tail, Msg, lists:reverse(EncodedValue) ++ Result, FieldMarshaller, EncodingRules).

%decode_fields(FieldIds, Message, OptionsRecord, Marshalled) ->
%	Message.
decode_fields([], Result, _OptionsRecord, _Marshalled) ->
	Result;
decode_fields([FieldId|Tail], Message, Options, Marshalled) ->
	EncodingRules = get_encoding_rules(Options, Message),
	FieldMarshalModule = Options#marshal_options.field_marshaller,
	if
		FieldMarshalModule =:= undefined ->
			Message;
		FieldMarshalModule =/= undefined ->
			{FieldValue, Rest} = FieldMarshalModule:unmarshal_field(FieldId, Marshalled, EncodingRules),
			decode_fields(Tail, erl8583_message:set(FieldId, FieldValue, Message), 
						  Options, Rest) 
	end.
	%{Value, UpdatedFields} = FieldMarshaller:unmarshal_field(FieldId, Fields),
	%UpdatedResult = erl8583_message:set(FieldId, Value, Result),
	%decode_fields(Tail, UpdatedFields, UpdatedResult, FieldMarshaller).

wrap_message(Options, Message, Marshalled) ->
	WrapperMarshalModule = Options#marshal_options.wrapping_marshaller,
	if
		WrapperMarshalModule =:= undefined ->
			Marshalled;
		WrapperMarshalModule =/= undefined ->
			WrapperMarshalModule:marshal_wrap(Message, Marshalled) 
	end.
	