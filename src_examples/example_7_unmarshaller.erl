% This unmarshaller can unmarshal a Postilion message with an
% unusual encoding of field 127.
-module(example_7_unmarshaller).

-export([unmarshal/1, unmarshal_init/2, unmarshal_field/3]).

unmarshal(Marshalled) ->
	MarshallingOptions = [{mti_marshaller, erl8583_marshaller_ascii},
						  {bitmap_marshaller, erl8583_marshaller_binary},
						  {field_marshaller, ?MODULE},
						  {init_marshaller, ?MODULE}],
	erl8583_marshaller:unmarshal(Marshalled, MarshallingOptions).

unmarshal_init(Message, Marshalled) ->
	MarshalledBin = erl8583_convert:ascii_hex_to_binary_list(Marshalled),
	{Message, MarshalledBin}.

% This function is called when a data element needs to be unmarshalled.
%
% Special handling for field 127.
% The length of field 127 is encoded in 6 bytes rather than 3.
% After extracting the value of field 127 we unmarshal it.
unmarshal_field(127, Marshalled, _EncodingRules) ->
	{LenStr, Rest} = lists:split(6, Marshalled),
	Len = list_to_integer(LenStr),
	{Value, MarshalledRem} = lists:split(Len, Rest),
	MarshallingOptions = [{bitmap_marshaller, erl8583_marshaller_binary},
						  {field_marshaller, ?MODULE},
						  {encoding_rules, example_7_encoding_rules}],
	Message = erl8583_marshaller:unmarshal(Value, MarshallingOptions),
	{Message, MarshalledRem, []};

% Use the binary marshaller to unmarshal binary fields and
% the ASCII marshaller to unmarshal all other fields.
unmarshal_field(FieldId, Marshalled, EncodingRules) ->
	case EncodingRules:get_encoding(FieldId) of
		{b, _, _} ->
			erl8583_marshaller_binary:unmarshal_field(FieldId, Marshalled, EncodingRules);
		{_, _, _} ->
			erl8583_marshaller_ascii:unmarshal_field(FieldId, Marshalled, EncodingRules)
	end.
