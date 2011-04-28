%% An example that demonstrates a minimal ISO 8583 client
%% that uses ASCII marshalling.
-module(example_3_client).
-include("erl8583/include/erl8583_field_ids.hrl").
-include("erl8583/include/erl8583_marshallers.hrl").
-export([test/0]).

test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set_mti("0800", Msg1),
	Msg3 = erl8583_message:set(3, "300000", Msg2),
	Msg4 = erl8583_message:set(24, "045", Msg3),
	Msg5 = erl8583_message:set(41, "11111111", Msg4),
	Msg6 = erl8583_message:set(42, "222222222222222", Msg5),
	Msg7 = erl8583_message:set(63, "This is a Test Message", Msg6),
	AsciiMessage = erl8583_marshaller_ascii:marshal(Msg7),
	{ok, Sock} = gen_tcp:connect("localhost", 8000, [list, {packet, 0}, {active, false}]),
	io:format("Sending:~n~s~n~n", [AsciiMessage]),
	
	% Our jPOS server expects a four digit length to be sent before the message.
	% We use an erl8583_convert function to create the header.
	LengthHeader = erl8583_convert:integer_to_string(length(AsciiMessage), 4),
	ok = gen_tcp:send(Sock, LengthHeader ++ AsciiMessage),
	AsciiResponse = do_recv(Sock, []),
	
	io:format("Received:~n~s~n", [AsciiResponse]),
	Response = erl8583_marshaller_ascii:unmarshal(AsciiResponse),
	
	% Display the MTI
	io:format("~nMTI: ~s~n", [erl8583_message:get(0, Response)]).	

do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
			UpdatedBs = Bs ++ B,
			if
				length(UpdatedBs) < 4 ->
					do_recv(Sock, UpdatedBs);
				true ->
					{LenStr, Rest} = lists:split(4, UpdatedBs),
					Len = list_to_integer(LenStr) + 4,
					if 
						Len >= length(UpdatedBs) ->
						   Rest;
						true ->
							do_recv(Sock, UpdatedBs)
					end
			end
    end.	
