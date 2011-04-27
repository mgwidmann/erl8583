-module(example_4_ascii_marshaller).
-include("erl8583/include/erl8583_field_ids.hrl").
-include("erl8583/include/erl8583_marshallers.hrl").
-export([test/0, marshal_end/2, unmarshal_init/2]).

marshal_end(_Message, Marshalled) ->
	Length = length(Marshalled),
	[Length div 256] ++ [Length rem 256] ++ Marshalled.

unmarshal_init(Message, Marshalled) ->
	[L1,L2|Rest] = Marshalled,
	L = length(Rest),
	L = 256 * L1 + L2,
	{Message, Rest}.

					 
test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set_mti("0800", Msg1),
	Msg3 = erl8583_message:set(3, "300000", Msg2),
	Msg4 = erl8583_message:set(24, "045", Msg3),
	Msg5 = erl8583_message:set(41, "11111111", Msg4),
	Msg6 = erl8583_message:set(42, "222222222222222", Msg5),
	Msg7 = erl8583_message:set(63, "This is a Test Message", Msg6),
	AsciiMessage = erl8583_marshaller:marshal(Msg7, ?MARSHALLER_ASCII ++ [{end_marshaller, ?MODULE}]),
	{ok, Sock} = gen_tcp:connect("localhost", 8583, [list, {packet, 0}, {active, true}]),
	ok = gen_tcp:send(Sock, AsciiMessage),
	receive {tcp, _, MarshalledResponse} -> MarshalledResponse end,
	Response = erl8583_marshaller:unmarshal(MarshalledResponse, ?MARSHALLER_ASCII ++ [{init_marshaller, ?MODULE}]),
	erl8583_message:get(0, Response).	
