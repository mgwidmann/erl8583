-module(example_6_unmarshaller).

-export([unmarshal/1, unmarshal_init/2, unmarshal_field/3]).

unmarshal(Marshalled) ->
	MarshallingOptions = [{mti_marshaller, erl8583_marshaller_ascii},
						  {bitmap_marshaller, erl8583_marshaller_binary},
						  {field_marshaller, ?MODULE},
						  {init_marshaller, ?MODULE}],
	erl8583_marshaller:unmarshal(Marshalled, MarshallingOptions).

unmarshal_init(Message, Marshalled) ->
	MarshalledBin = erl8583_convert:ascii_hex_to_binary(Marshalled),
	{Message, binary_to_list(MarshalledBin)}.

unmarshal_field(127, Marshalled, _EncodingRules) ->
	{LenStr, Rest} = lists:split(6, Marshalled),
	Len = list_to_integer(LenStr),
	{Value, MarshalledRem} = lists:split(Len, Rest),
	{Value, MarshalledRem, []};
unmarshal_field(FieldId, Marshalled, EncodingRules) ->
	case EncodingRules:get_encoding(FieldId) of
		{b, _, _} ->
			erl8583_marshaller_binary:unmarshal_field(FieldId, Marshalled, EncodingRules);
		{_, _, _} ->
			erl8583_marshaller_ascii:unmarshal_field(FieldId, Marshalled, EncodingRules)
	end.
