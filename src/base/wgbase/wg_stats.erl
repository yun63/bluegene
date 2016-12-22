%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng@gmail.com
%%% @date 2011.04.07
%%% @doc 统计虚拟机内部运行状态
%%% @end
%%%----------------------------------------------------------------------
-module(wg_stats).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("wg_internal.hrl").

-export([i/0]).

%% @doc 信息
i() ->
    {{input, IoInput}, {output, IoOutput}} = erlang:statistics(io),
    CheckIos = erlang:system_info(check_io),
    MaxFds = proplists:get_value(max_fds, CheckIos),
    Memory = erlang:memory(),
    MemoryList =
    [begin
        MKey2 = lists:concat([memory, "_", MKey]),
        {?S2A(MKey2), MVal}
    end || {MKey, MVal} <- Memory],

    MemoryList ++ 
    [
    {process_count, erlang:system_info(process_count)},
    {run_queue, erlang:statistics(run_queue)},
    {context_switch, element(1, erlang:statistics(context_switches))},
    {io_input_total, IoInput},
    {io_output_total, IoOutput},
    {max_fds, MaxFds},
    {wordsize, erlang:system_info(wordsize)}].
