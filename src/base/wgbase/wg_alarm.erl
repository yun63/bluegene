%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @doc 用来监控节点运行情况(基于sasl/alarm_handler)
%%%     包含内存,cpu,gc以及自定义警告
%%%     alarm_id包含:long_gc, large_heap, busy_port, busy_dist_port, player_crash
%%%
%%%----------------------------------------------------------------------
-module(wg_alarm).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("wg_internal.hrl").
-behaviour(gen_event).

-export([start/0]).
-export([player_crash/1]).
-export([i/0, register/2, unregister/1]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
                            terminate/2, code_change/3]).

%% 内部状态
-record(state, {
        handler = dict:new()    % alarm处理
    }).

-define(SERVER, alarm_handler).
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

%% @doc 启动服务
start() ->
    ?DEBUG(?_U("启动wg_alarm服务"), []),
    L = gen_event:which_handlers(?SERVER),
    ?IF(lists:member(?MODULE, L), ok,
        ok = gen_event:add_handler(?SERVER, ?MODULE, [])).

%% @doc 玩家crash
player_crash(Data) ->
    alarm_handler:set_alarm({player_crash, Data}).

%% @doc 运行信息
i() ->
    gen_event:call(?SERVER, ?MODULE, i).

%% @dco 注册处理函数
register(AlarmId, Fun) when is_function(Fun, 1); is_tuple(Fun) ->
    gen_event:notify(?SERVER, {register, AlarmId, Fun}).

%% @doc 取消注册
unregister(AlarmId) ->
    gen_event:notify(?SERVER, {unregister, AlarmId}).

%%
%% gen_server callbacks
%%
init(_Args) ->
    ?DEBUG(?_U("*****启动wg_alarm")),
    erlang:system_monitor(self(), ?OPTIONS),
    ok = do_start_os_mon(),
    {ok, #state{}}.

handle_event({register, AlarmId, Fun}, #state{handler = Handler} = State) ->
    Handler2 = dict:store(AlarmId, Fun, Handler),
    {ok, State#state{handler = Handler2}};
handle_event({unregister, AlarmId}, #state{handler = Handler} = State) ->
    Handler2 = dict:erase(AlarmId, Handler),
    {ok, State#state{handler = Handler2}};

handle_event({set_alarm, Alarm}, State) ->
    do_handle_alarm(Alarm, State),
    {ok, State};
handle_event({clear_alarm, _Alarm}, State) ->
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

handle_call(i, State) ->
    Info = do_i(State),
    {ok, Info, State};
handle_call(_Msg, State) ->
    {noreply, State}.

handle_info({monitor, GcPid, long_gc, Info}, State) ->
    alarm_handler:set_alarm({long_gc, {GcPid, Info}}),
    {ok, State};
handle_info({monitor, GcPid, large_heap, Info}, State) ->
    alarm_handler:set_alarm({large_heap, {GcPid, Info}}),
    {ok, State};
handle_info({monitor, SusPid, busy_port, Port}, State) ->
    alarm_handler:set_alarm({busy_port, {SusPid, Port}}),
    {ok, State};
handle_info({monitor, SusPid, busy_dist_port, Port}, State) ->
    alarm_handler:set_alarm({busy_dist_port, {SusPid, Port}}),
    {ok, State};
handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ?ERROR(?_U("结束:~p"), [_Reason]),
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
    
%%---------------
%% internal API
%%---------------

%% 启动os_mon
do_start_os_mon() ->
    case application:start(os_mon) of
        ok ->
            ok;
        {error, {already_started, _}} ->
            ok;
        _Other ->
            throw({stop, _Other})
    end.

%% 内部信息
do_i(#state{handler = Handler}) ->
    dict:to_list(Handler).

%% 处理alarm
do_handle_alarm({AlarmId, Data}, #state{handler = Handler}) ->
    case dict:find(AlarmId, Handler) of
        {ok, Fun} ->
            try 
                do_call_fun(Fun, Data)
            catch
                Type:Error ->
                    ?WARN(?_U("调用警告{~p,~p}处理函数出错~p:~p"), 
                        [AlarmId, Data, Type, Error])
            end;
        error ->
            ok
    end.

do_call_fun(Fun, Data) when is_function(Fun, 1) ->
    catch Fun(Data);
do_call_fun({M, F, A}, Data) ->
    catch erlang:apply(M, F, A ++ [Data]).

%%-------------
%% EUnit TEST
%%-------------

-ifdef(EUNIT).
-endif.
