%%%--------------------------------------
%%% @Module  : game_ctrl
%%% @Author  : yun63
%%% @Created : 2015-3-7
%%% @Description : 控制服务器启动和一些rpc调用
%%%--------------------------------------
-module(game_ctrl).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0, 
		 stop/0,
		 start_applications/1,
		 info/0
	]).

-define(SERVER_APPS, [gateway, area, world]).


%%
%% API Functions
%%

%%游戏服务器
start()->
    io:format("======= Stasrt Game Server ========~n"),
    try
        ok = start_applications(?SERVER_APPS)
    after
        timer:sleep(100)
    end.

%%停止游戏服务器
stop() ->
    ok = stop_applications(?SERVER_APPS),
    erlang:halt().


info() ->
    SchedId      = erlang:system_info(scheduler_id),
    SchedNum     = erlang:system_info(schedulers),
    ProcCount    = erlang:system_info(process_count),
    ProcLimit    = erlang:system_info(process_limit),
    ProcMemUsed  = erlang:memory(processes_used),
    ProcMemAlloc = erlang:memory(processes),
    MemTot       = erlang:memory(total),
    io:format( "abormal termination:
                       ~n   Scheduler id:                         ~p
                       ~n   Num scheduler:                        ~p
                       ~n   Process count:                        ~p
                       ~n   Process limit:                        ~p
                       ~n   Memory used by erlang processes:      ~p
                       ~n   Memory allocated by erlang processes: ~p
                       ~n   The total amount of memory allocated: ~p
                       ~n",
                            [SchedId, SchedNum, ProcCount, ProcLimit,
                             ProcMemUsed, ProcMemAlloc, MemTot]),
	ok.


%%
%% Local Functions
%%

%%############辅助调用函数##############
manage_applications(Iterate, Do, Undo, SkipError, ErrorTag, Apps) ->
    Iterate(fun (App, Acc) ->
                    case Do(App) of
                        ok -> [App | Acc];%合拢
                        {error, {SkipError, _}} -> Acc;
                        {error, Reason} ->
                            lists:foreach(Undo, Acc),
                            throw({error, {ErrorTag, App, Reason}})
                    end
            end, [], Apps),
    ok.

start_applications(Apps) ->
    manage_applications(fun lists:foldl/3,
                        fun application:start/1,
                        fun application:stop/1,
                        already_started,
                        cannot_start_application,
                        Apps).

stop_applications(Apps) ->
    manage_applications(fun lists:foldr/3,
                        fun application:stop/1,
                        fun application:start/1,
                        not_started,
                        cannot_stop_application,
                        Apps).
