-module(example_5_amex_message).

-export([test/0]).

test() ->
	MarshalStr = "F1F1F0F07030254000408000F1F5F3F7F0F0F1F2F3F4F5F6F1" ++ 
					 "F2F3F4F5F0F0F4F0F0F0F0F0F0F0F0F0F0F0F0F1F0F0F0" ++ 
					 "F0F0F0F0F1F0F9F0F1F0F0F0F0F0F0F0F0F1F3F0F1F8F4" ++ 
					 "F0F1F0F1F1F5F0F6F0F0F1F2F0F1F8F0F1F2F3F4F0F7F4" ++ 
					 "F2F0F0F0F0F0F0F0F1F2F3F4F5F6F7F8F8F4F0",
	Marshalled = binary_to_list(erl8583_convert:ascii_hex_to_binary(MarshalStr)),
	Message = example_5_unmarshaller:unmarshal(Marshalled),
	erl8583_message:get_fields(Message).


	
