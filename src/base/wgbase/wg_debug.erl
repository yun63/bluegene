%%%----------------------------------------------------------------------
%%%
%%% @author BigBlackBear
%%% @doc 用来做生成调试信息
%%%
%%%----------------------------------------------------------------------
-module(wg_debug).

-include("wg.hrl").

-export([run/5]).
-export([test/1]).
-export([perf_time_start/1, perf_time_check/4]).

run(player, Id, Mod, Fun, Args) ->
    player_server:call(Id, {wg_run, Mod, Fun, Args});
run(map, Id, Mod, Fun, Args) ->
    map_server:call(Id, {wg_run, Mod, Fun, Args});
run(_, _, _, _, _) ->
    undefined.

test(0) ->
    wg_tools:fprof_start(new),
    start_map(1, 1),
    wg_tools:fprof_stop(),
    ok;
test(N) when is_integer(N) ->
    {Mega1, Sec1, Micro1} = erlang:now(),
    start_map(N, Micro1),
    {Mega2, Sec2, Micro2} = erlang:now(),
    io:format("test cost ~p ms ~n", [((Mega2 - Mega1) * 1000000000 + (Sec2 - Sec1) * 1000 + (Micro2 - Micro1) div 1000)]),
    ok;
test(_) ->
    undefined.

start_map(N, S) ->
    [begin
         map_sup:start_map(13001, none, {single, Owner})
    end || Owner <- lists:seq(S+1, S+N)].


% 记录开始测试时间
perf_time_start(Flag) when is_atom(Flag) ->
    erlang:put(Flag, erlang:now()).

perf_time_check(Flag, Time, Format, List) when is_atom(Flag) ->
    try
        {Mega2, Sec2, Micro2} = erlang:now(),
        {Mega1, Sec1, Micro1} = erlang:get(Flag),
        Diff =
            ((Mega2 - Mega1) * 1000000000 +
             (Sec2 - Sec1) * 1000 +
             (Micro2 - Micro1) div 1000),
        if
            (Diff > Time) -> ?WARN("~p Overtime(~p, ~p) for " ++ Format, [Flag, Diff, Time] ++ List);
            true -> ok
        end
    catch
        _:_E ->
            ?WARN("PERF_TIME_CHECK ~p ERROR ~p", [Flag, _E])
    end.
