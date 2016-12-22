%%%----------------------------------------------------------------------
%%%
%%% @copyright wg 
%%%
%%% @author songze.me@gmail.com
%%% @doc helper module for some tools
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(wg_tools).
-author('songze.me@gmail.com').
-vsn('0.1').
-include("wg_internal.hrl").

-compile([export_all]).

%%--------------
%% 关于进程
%%--------------

%% @doc 进程gc相关设置
proc_gc(Pid) when is_pid(Pid) ->
    {garbage_collection,
        [{min_bin_vheap_size, MinBinVHeap},
            {min_heap_size, MinHeap},
            {fullsweep_after, FullSweep},
            {minor_gcs, MinorGcs}]
    } = erlang:process_info(Pid, garbage_collection),
    io:format(
    ?_U(
    "最小vheap   : ~b\n"
    "最小heap    : ~b\n"
    "fullsweep   : ~b\n"
    "次要gc数    : ~b\n"),
    [MinBinVHeap, MinHeap, FullSweep, MinorGcs]).

%% @doc 统计某个进程的指定Time秒内gc次数,通过trace实现
proc_gc_statis(Pid, Time) when is_pid(Pid), is_integer(Time) ->
    ?ASSERT(Pid =/= self()),
    ?IF(erlang:is_process_alive(Pid), ok, throw(noproc)),
    Fun =
    fun() ->
        1 = erlang:trace(Pid, true, [garbage_collection, timestamp]),
        timer:sleep(Time * 1000),
        1 = erlang:trace(Pid, false, [all]),
        % 统计
        List = do_recv_gc_msg(Pid),
        {GcCount, GcTime} = do_proc_gc_statis(List),
        io:format(
        ?_U(
        "=====================================\n"
        "进程~p:\n"
        "~b秒内,gc次数:~p 用时:~p(微秒)\n"
        ),
        [Pid, Time, GcCount, GcTime]),
        proc_gc(Pid)
    end,
    _ = spawn_link(Fun),
    ok.
%% 进行gc结果统计
do_proc_gc_statis([]) ->
    {0, 0};
do_proc_gc_statis(L) ->
    {_, L2} =
    lists:foldl(
    fun
        ({gc_start, _, _} = Prev, {none, Acc}) ->
            {Prev, Acc}; 
        ({gc_end, _Info, TimeEnd}, {Prev, Acc}) ->
            {gc_start, _, TimeStart} = Prev,
            {none, [{1, timer:now_diff(TimeEnd, TimeStart)} | Acc]}
    end, {none, []}, L),
    {CountList, TimeList} = lists:unzip(L2),
    GcCount = length(CountList),
    GcTime = lists:sum(TimeList),
    {GcCount, GcTime}.

%% 接收gc消息
do_recv_gc_msg(Pid) ->
    lists:reverse(do_recv_gc_msg(Pid, [])).
do_recv_gc_msg(Pid, Acc) ->
    receive 
        {trace_ts, Pid, gc_start, Info, Time} ->
            do_recv_gc_msg(Pid, [{gc_start, Info, Time} | Acc]);
        {trace_ts, Pid, gc_end, Info, Time} ->
            do_recv_gc_msg(Pid, [{gc_end, Info, Time} | Acc])
    after 
        0 ->
            Acc
    end.

%% @doc 垃圾回收
gc(Pid) ->
    erlang:garbage_collect(Pid).

%% @doc 所有进程垃圾回收
gc() ->
    [erlang:garbage_collect(Pid) || Pid <- processes()],
    ok.

%%--------------
%% 关于etop
%%--------------
%% @doc etop启动
etop_start() ->
    spawn(fun() ->
        etop:start([{output, text}, {interval, 2}, 
                    {lines, 10}, {sort, msg_q}])
    end).

%% @doc 设置etop按运行时间排序
etop_sort_by_runtime() ->
    etop:config(sort, runtime).

%% @doc 设置etop按reduction排序
etop_sort_by_reduction() ->
    etop:config(sort, reductions).

%% @doc 设置etop按memory排序
etop_sort_by_memory() ->
    etop:config(sort, memory).

%% @doc 设置etop按msg排序
etop_sort_by_msg() ->
    etop:config(sort, msg_q).

%% @doc etop停止
etop_stop() ->
    etop:stop().

%%---------------
%% 关于eprof
%%---------------

%% @doc eprof开始
eprof_start(Pid) when is_pid(Pid) ->
    eprof:start_profiling([Pid]);
eprof_start(Pids) when is_list(Pids) ->
    eprof:start_profiling(Pids).

%% @doc eprof完成
eprof_stop() ->
    Analyse = lists:concat(["eprof-", wg_util:now_sec(), ".analysis"]),
    ok = eprof:log(Analyse),
    eprof:stop_profiling(),
    ok = eprof:analyze(),
    io:format(?_U("eprof分析完成，结果:~s\n"), [Analyse]),
    ok.

%%---------------
%% 关于fprof
%%---------------

%% @doc fprof开始
-define(FPROF_TRACE_FILE, "/tmp/fprof.trace").
fprof_start() ->
    fprof_start(self()).

%% @doc fprof指定的进程:
%% all | existing | new | pid()
fprof_start(Procs) ->
    fprof:trace([start, {file, ?FPROF_TRACE_FILE}, {procs, Procs}]).

%% @doc fprof完成
fprof_stop() ->
    ok = fprof:trace(stop),
    ok = fprof:profile({file, ?FPROF_TRACE_FILE}),
    Analyse = lists:concat(["fprof-", wg_util:now_sec(), ".analysis"]),
    % {sort, own}
    %ok = fprof:analyse([{dest, Analyse}, {details, true}, {totals, true}, {sort, own}]),
    ok = fprof:analyse([{dest, Analyse}, {details, true}, {totals, true}, {sort, own}]),
    io:format(?_U("fprof分析完成，结果:~s\n"), [Analyse]),
    ok.

%%---------------------
%% trace一个进程的消息
%%---------------------

%% @doc 开始trace
trace_start(Pid) ->
    dbg:tracer(),
    dbg:p(Pid, all),
    ok.

%% @doc 结束trace
trace_stop() ->
    dbg:stop(),
    dbg:stop_clear().
