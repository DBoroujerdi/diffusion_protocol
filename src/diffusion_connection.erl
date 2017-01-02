-module(diffusion_connection).

-include("diffusion_common.hrl").
-include("diffusion_protocol.hrl").

-export([decode/1]).


%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

%% @doc Decodes the connection response is sent through from the server to the
%%      client over the websocket. It is sent as a text frame begining with
%%      the Diffusion™ protocol version, followed by the string "100" for new
%%      connections and "105" for reconnections, followed by the client ID.

-spec decode(binary()) -> #diffusion_connection{}.

decode(<<Version:1/bytes, ?FD, ConnectionType:3/bytes, ?FD, ClientId/binary>>) ->
    #diffusion_connection{type=connection_type(ConnectionType),
                          client_id=ClientId,
                          version=version(Version)}.



%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

%% todo: what are the differences between 4 and 5?
version(<<"4">>) -> 4;
version(<<"5">>) -> 5.

connection_type(<<"100">>) -> initial;
connection_type(<<"105">>) -> reconnection.



%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

decode_connection_response_test() ->
    Bin = <<"4\x{2}100\x{2}C8D4048FA5712A3A-006740E900000004">>,
    Con = decode(Bin),
    ?assertEqual(#diffusion_connection{type=initial,
                                       client_id= <<"C8D4048FA5712A3A-006740E90000000">>,
                                       version=4}, Con).

-endif.
