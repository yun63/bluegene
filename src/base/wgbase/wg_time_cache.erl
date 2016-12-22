%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng@gmail.com
%%% @doc 缓存一个低精度的时间,其保存在ets中.
%%%  大大减少对erlang:now/0的调用
%%%
%%%----------------------------------------------------------------------
-module(wg_time_cache).
-author('litaocheng@gmail.com').
-include("wg_internal.hrl").
-include("const.hrl").
-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([start/1, stop/0]).
-export([now/0, now_sec/0, now_ms/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).

-record(state, {
        precision           % 精度(ms)
    }).

-define(NAME, ?MODULE).
-define(TABLE, ?MODULE).
        
%% @doc 启动服务,参数为精度(ms),如100表示每隔100ms更新一次时间
%% 允许的范围为10-1000
start_link() ->
    start_link(50).
start_link(Precision) when Precision >= 10, Precision =< 1000 ->
    gen_server:start_link({local, ?NAME}, ?MODULE, Precision, []).

%% @doc 启动服务
start(Precision) ->
    gen_server:start({local, ?NAME}, ?MODULE, Precision, []).

%% @doc 停止
stop() ->
    gen_server:call(?NAME, stop).

%% @doc 获取now时间和erlang:now/0格式相同
now() ->
    case catch ets:lookup_element(?TABLE, now, 2) of
        {'EXIT', {badarg, _}} ->
            ?NONE;
        Val ->
            Val
    end.

%% @doc 获取当前秒
now_sec() ->
    case catch ets:lookup_element(?TABLE, now_sec, 2) of
        {'EXIT', {badarg, _}} ->
            ?NONE;
        Val ->
            Val
    end.

%% @doc 获取当前毫秒
now_ms() ->
    case catch ets:lookup_element(?TABLE, now_ms, 2) of
        {'EXIT', {badarg, _}} ->
            ?NONE;
        Val ->
            Val
    end.
    
%%
%% gen_server callbacks
%%
init(Precision) ->
    erlang:process_flag(trap_exit, true),
    erlang:process_flag(priority, high),
    ?TABLE = ets:new(?TABLE, [set, public, named_table, {read_concurrency, true}]),
    start_update_timer(Precision),
    State = do_update_time(#state{precision = Precision}),
    {ok, State}.
    
handle_call(stop, _From, State) ->
    {reply, ok, normal, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(update_time, #state{precision = Precision} = State) ->
    start_update_timer(Precision),
    State2 = do_update_time(State),
    {noreply, State2};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
    
%%-----------------------------------------------------------------------------
%% internal API
%%-----------------------------------------------------------------------------

%% 启动更新timer
start_update_timer(Time) ->
    erlang:send_after(Time, self(), update_time).

%% 更新时间
do_update_time(State) ->
    Now = {Mega, Sec, Micro} = erlang:now(),
    NowSec = Mega * 1000000 + Sec,
    NowMs = NowSec * 1000 + Micro div 1000,
    true = ets:insert(?TABLE, [{now, Now}, {now_sec, NowSec}, {now_ms, NowMs}]),
    State.

%%-------------
%% EUnit TEST
%%-------------

-ifdef(EUNIT).
-endif.
