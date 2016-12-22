%%%----------------------------------------------------------------------
%%%
%%% @author BigBlackBear
%%% @date  2012.04.15
%%% @doc 随机数
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(rand).

-export([pass/1, pass/2]).

-include("const.hrl").

diff(P) ->
    P - game_misc:rand().

pass(P) ->
    diff(P) >= 0.

pass(P, D) ->
    {S, T, P2} =
    case get({rand_adjust, P, D}) of
        {S0, T0} ->
            % 这里加调整项
            {S0, T0, P};
        _Any -> 
            {0, 0, P}
    end,
    Pass = pass(P2),
    S2 =
    if
        (Pass) -> S;
        true -> S + 1
    end,
    T2 = T + 1,
    put({rand_adjust, P, D}, {S2, T2}),
    Pass.