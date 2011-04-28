%% Code to show how to write a marshaller for iso8583py.
%% This code effectively extends the ASCII marshaller by adding an
%% end_marshaller to the marshaller.
-module(example_4_marshaller).

-include_lib("erl8583/include/erl8583_marshallers.hrl").

-export([marshal/1, marshal_end/2]).

% Our marshal function uses the ASCII marshaller but specifies that the
% marshal_end function in this module must be called after marshalling
% all data elements.
marshal(Message) ->
	MarshallingOptions = ?MARSHALLER_ASCII ++ [{end_marshaller, ?MODULE}],
	erl8583_marshaller:marshal(Message, MarshallingOptions).

% After marshalling the message, we prepend the message with the length of
% the message encoded in two bytes.
marshal_end(_Message, Marshalled) ->
	Length = length(Marshalled),
	[Length div 256, Length rem 256] ++ Marshalled.
