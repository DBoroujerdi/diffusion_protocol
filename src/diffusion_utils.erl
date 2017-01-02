-module(diffusion_utils).

-export([join/2]).

join(_, []) -> [];
join(_, [_] = L) -> L;
join(Sep, L0) ->
    [_|L1] = lists:reverse(join(Sep, L0, [])),
    L1.

join(_, [], Acc) -> Acc;
join(Sep, [H], Acc) -> [H] ++ [Sep] ++ Acc;
join(Sep, [H|T], Acc) ->
    join(Sep, T, [H] ++ [Sep] ++ Acc).


%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

join_test() ->
    ?assertEqual([a, x, b, x, c], join(x, [a, b, c])),
    ?assertEqual([a], join(x, [a])),
    ?assertEqual([], join(x, [])).

-endif.
