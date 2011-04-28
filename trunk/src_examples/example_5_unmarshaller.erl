-module(example_5_unmarshaller).

-export([unmarshal/1, unmarshal_field/3]).

unmarshal(Marshalled) ->
	MarshallingOptions = [{mti_marshaller, erl8583_marshaller_ebcdic},
						  {bitmap_marshaller,  erl8583_marshaller_binary},
						  {field_marshaller,  ?MODULE}],
	erl8583_marshaller:unmarshal(Marshalled, MarshallingOptions).

unmarshal_field(FieldId, Marshalled, EncodingRules) ->
	case EncodingRules:get_encoding(FieldId) of
		{b, _, _} ->
			erl8583_marshaller_binary:unmarshal_field(FieldId, Marshalled, EncodingRules);
		{_, _, _} ->
			erl8583_marshaller_ebcdic:unmarshal_field(FieldId, Marshalled, EncodingRules)
	end.

			
