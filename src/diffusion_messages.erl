-module(diffusion_messages).

%% API exports
-export([decode/1,
         encode/1]).

-include("diffusion_protocol.hrl").
-include("diffusion_common.hrl").


%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

%% @doc Decode a message binary in the form - TH...D...
%%      Where T is the message-type byte, H is optional header bytes seperated
%%      by field delimiters FD and D is the data bytes also seperated by field
%%      delimiters.

-spec decode(binary()) -> #diffusion_message{} |
                          {error, atom()}.

decode(<<>>) ->
    {error, empty_binary};
decode(<<Type:1/bytes, Rest/binary>>) ->
    case binary:split(Rest, <<?RD>>) of
        [Empty] when byte_size(Empty) =:= 0 ->
            {error, no_data};
        [Data] ->
            #diffusion_message{type=Type, data=split(Data), headers=[]};
        [Headers, Data] ->
            #diffusion_message{type=Type, data=split(Data), headers=split(Headers)}
    end.


%% @doc Encode a diffusion message as a binary of the form TH...D...
%%      Where T is the message-type byte, H is optional header bytes seperated
%%      by field delimiters FD and D is the data bytes also seperated by field
%%      delimiters.

-spec encode(#diffusion_message{}) -> binary() | {error, atom()}.

encode(#diffusion_message{type=Type, data=Data, headers=[]}) ->
    iolist_to_binary([Type, encode_data(Data)]);
encode(#diffusion_message{type=Type, data=Data, headers=Headers}) ->
    iolist_to_binary([Type, encode_data(Headers), ?RD, encode_data(Data)]).



%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

encode_data(DataList) ->
    diffusion_utils:join(?FD, DataList).

split(Bin) ->
    binary:split(Bin, <<?FD>>).




%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(DELTA_TYPE, <<"\x{15}">>).

encode_test() ->
    Bin = encode(#diffusion_message{type=?DELTA_TYPE,
                                    data=[<<"foo">>, <<"bar">>],
                                    headers=[]}),

    Expected = iolist_to_binary([?DELTA_TYPE, <<"foo">>, ?FD, <<"bar">>]),

    ?assertEqual(Bin, Expected).

encode_with_headers_test() ->
    Bin = encode(#diffusion_message{type=?DELTA_TYPE,
                                    data=[<<"foo">>, <<"bar">>],
                                    headers=[<<"h1">>, <<"h2">>]}),

    Expected = iolist_to_binary([?DELTA_TYPE,
                                 <<"h1">>, ?FD,
                                 <<"h2">>, ?RD,
                                 <<"foo">>, ?FD,
                                 <<"bar">>]),

    ?assertEqual(Bin, Expected).

split_test() ->
    [<<"foo">>, <<"bar">>] = split(<<"foo\x{2}bar">>),
    [<<"foobar">>] = split(<<"foobar">>).

decode_test() ->
    Msg = iolist_to_binary([?DELTA_TYPE, <<"topicName", ?FD,
                                           "topicAlias", ?RD,
                                           "Data1", ?FD,
                                           "Data2">>]),

    #diffusion_message{type = ?DELTA_TYPE,
                       headers = [<<"topicName">>, <<"topicAlias">>],
                       data = [<<"Data1">>, <<"Data2">>]} = decode(Msg).

-endif.
