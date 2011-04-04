-define(MARSHALLER_ASCII, [{field_marshaller, erl8583_marshaller_ascii}, 
						   {bitmap_marshaller, erl8583_marshaller_ascii}]).

-define(MARSHALLER_BINARY, [{field_marshaller, erl8583_marshaller_binary}, 
						    {bitmap_marshaller, erl8583_marshaller_binary}]).
