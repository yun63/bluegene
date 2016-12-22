%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date 2011-12-27
%%% @doc 关于游戏中的报警
%%%
%%%----------------------------------------------------------------------
-module(game_alarm).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("const.hrl").

-export([init/0]).
-export([on_player_crash/1, on_long_gc/1, on_large_heap/1,
        on_busy_port/1, on_busy_dist_port/1,
        on_proc_memory_high/1, system_memory_high_watermark/1
    ]).

-ifdef(TEST).
-define(OPTIONS, 
    [{long_gc, 2000},
    {large_heap, 4 * 1024 * 1024},
    busy_port,
    busy_dist_port]).
-else.
-define(OPTIONS, 
    [{long_gc, 3000},               % gc达到3秒
    {large_heap, 5 * 1024 * 1024},  % 40M
    busy_port,
    busy_dist_port]).
-endif.

%% @doc 初始化报警
init() ->
    wg_alarm:start(),
    wg_alarm:register(player_crash, {?MODULE, on_player_crash, []}),
    wg_alarm:register(long_gc, {?MODULE, on_long_gc, []}),
    wg_alarm:register(large_heap, {?MODULE, on_large_heap, []}),
    wg_alarm:register(busy_port, {?MODULE, on_busy_port, []}),
    wg_alarm:register(busy_dist_port, {?MODULE, on_busy_dist_port, []}),
    wg_alarm:register(process_memory_high_watermark, {?MODULE, on_proc_memory_high, []}),
    wg_alarm:register(system_memory_high_watermark, {?MODULE, system_memory_high_watermark, []}),
    ok.

%% @doc 当玩家断线退出
-ifdef(PRODUCT).
-define(PLAYR_ALARM_CD, 'player_alarm_cd').
on_player_crash({Id, Reason, TerminateReason}) ->
    case check_cd(?PLAYR_ALARM_CD, 2) of
        true ->
            set_cd(?PLAYR_ALARM_CD),
            Str = io_lib:format("玩家:~p逻辑进程出错:~p(~p)", 
                [Id, Reason, TerminateReason]),
            game_misc:alarm_mail(Str, fun(_) -> ok end);
        false ->
            ok
    end.
-else.
on_player_crash({_Id, _Reason, _TerminateReason}) ->
    ok.
-endif.

%% @doc 当gc比较长时
-define(LONG_GC_CD, '!long_gc_cd').
on_long_gc({Pid, _}) ->
    case check_cd(?LONG_GC_CD, 5) of
        true ->
            set_cd(?LONG_GC_CD),
            Str = io_lib:format("进程:~pgc时间过长:~p", 
                [Pid, erlang:process_info(Pid)]),
            game_misc:alarm_mail(Str, fun(_) -> ok end);
        false ->
            ok
    end.

%% @doc 当某个进程想要分配较大内存时
on_large_heap({Pid, Info}) ->
    case check_cd(on_large_heap, 5) of
        true ->
            set_cd(on_large_heap),
            Str = io_lib:format("进程:~p想要分配过大的内存:~n~p~n~n~p", 
                [Pid, Info, erlang:process_info(Pid)]),
            game_misc:alarm_mail(Str, fun(_) -> ok end);
        false ->
            ok
    end.

%% @doc 当busy port
-define(BUSY_PORT_CD, '!busy_port_cd').
on_busy_port({Port, _}) ->
    case check_cd(?BUSY_PORT_CD, 5) of
        true ->
            set_cd(?BUSY_PORT_CD),
            Str = io_lib:format("port:~p忙碌~n~p", 
                [Port, wg_util:get_port_info(Port)]),
            game_misc:alarm_mail(Str, fun(_) -> ok end);
        false ->
            ok
    end.

%% @doc 当dist port busy
-define(BUSY_DIST_PORT_CD, '!busy_dist_port_cd').
on_busy_dist_port({Port, _}) ->
    case check_cd(?BUSY_DIST_PORT_CD, 5) of
        true ->
            set_cd(?BUSY_DIST_PORT_CD),
            Str = io_lib:format("dist port:~p忙碌~n~p", 
                [Port, wg_util:get_port_info(Port)]),
            game_misc:alarm_mail(Str, fun(_) -> ok end);
        false ->
            ok
    end.

%% @doc 当proc内存过大
-define(PROC_MEMORY_HIGH_CD, '!proc_memory_high_cd').
on_proc_memory_high(Pid) ->
    case check_cd(?PROC_MEMORY_HIGH_CD, 5) of
        true ->
            set_cd(?PROC_MEMORY_HIGH_CD),
            erlang:garbage_collect(Pid),
            Str = io_lib:format("进程:~p占用内存过高~n~p", 
                [Pid, erlang:process_info(Pid)]),
            game_misc:alarm_mail(Str, fun(_) -> ok end);
        false ->
            ok
    end.

%% @doc 当系统内存过大
system_memory_high_watermark(_) ->
    case check_cd(system_memory_high_watermark, 1) of
        true ->
            set_cd(system_memory_high_watermark),
            Str = io_lib:format("系统占用内存过高", []),
            game_misc:alarm_mail(Str, fun(_) -> ok end);
        false ->
            ok
    end.

%%---------------------
%% internal API
%%---------------------

%% 检测cd
check_cd(Key, Time) ->
    Now = wg_util:now_sec(),
    Last = world_data:get(Key),
    Last =:= ?NONE orelse Now - Last >= Time.

%% 设置cd
set_cd(Key) ->
    Now = wg_util:now_sec(),
    world_data:set(Key, Now).
