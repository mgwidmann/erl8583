% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

%% @author CA Meijer
%% @copyright 2011 CA Meijer
%% @doc This module constructs an EBCDIC binary representation of the bit map of
%%      an iso8583message() field.
-module(erl8583_marshaller_ebcdic_bitmap).

%%
%% Include files
%%
%% @headerfile "../include/erl8583_types.hrl"
-include("erl8583_types.hrl").

%%
%% Exported Functions
%%
-export([marshal/1]).

%%
%% API Functions
%%

%% @doc Constructs an EBCDIC binary representation of the
%%      bitmap for an iso8583message().
%%
%% @spec marshal(iso8583message()) -> binary()
-spec(marshal(iso8583message()) -> binary()).

marshal(Message) ->
	erl8583_convert:ascii_to_ebcdic(erl8583_marshaller_ascii_bitmap:marshal(Message)).


%%
%% Local Functions
%%

