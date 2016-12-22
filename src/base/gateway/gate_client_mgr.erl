%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2011.01.02
%%% @doc gate_client manager
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(gate_client_mgr).
-author("litaocheng@gmail.com").
-vsn('0.1').
-behaviour(gen_server).
-include("wg.hrl").
-include("const.hrl").
-include("gate_internal.hrl").

-export([start_link/0]).
-export([i/0, all/0, all_pid/0, count/0, get/1, add/2, delete/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).
                            
%% @doc 启动gate_client_mgr
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc 信息
i() ->
    All = all(),
    [{count, count()},
    {list, All}].

%% @doc 获取所有已经登录的玩家列表
all() ->
    ets:tab2list(?TAB). 

%% @doc 获取所有玩家pid
all_pid() ->
    [Pid || {_Id, Pid} <- all()].

%% @doc 查询id对应的进程
get(Id) when is_integer(Id) ->
    case ets:lookup(?TAB, Id) of
        [] ->
            ?NONE;
        [{Id, Pid}] ->
            Pid
    end.

%% @doc 总计数
count() ->
    ets:info(?TAB, size).

%% @doc 向管理器中添加gate client
add(Id, Pid) ->
    case ets:insert_new(?TAB, {Id, Pid}) of
        true ->
            ok;
        false ->
            ?ERROR(?_U("mgr中玩家已经存在:~p"), [Id]),
            {error, gate_client_exist}
    end.

%% @doc 从管理器中删除gate client
delete(Id) when is_integer(Id) ->
    true = ets:delete(?TAB, Id),
    ok.

%%----------------------
%% gen_server callbacks
%%----------------------
init(_Args) ->    
    erlang:process_flag(trap_exit, true),
    % 创建table
    ?TAB = ets:new(?TAB, [set, public, named_table,
            {keypos, 1}, 
            ?ETS_CONCURRENCY
            ]),
    {ok, []}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?DEBUG(?_U("进程停止:~p"), [_Reason]),
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
    
%%
%% internal API
%%

%%------------
%% EUNIT test
%%------------
-ifdef(EUNIT).

basic_test_() ->
    {spawn, {inorder, {setup,
    fun() -> start_link() end,
    fun({ok, Pid}) -> erlang:exit(Pid, kill) end,
    [
        ?_assertEqual(ok, add(1, 1)),
        ?_assertEqual({error, gate_client_exist}, add(1, 2)),
        ?_assertEqual([{1, 1}], all()),

        ?_assertEqual(ok, delete(1)),
        ?_assertEqual([], all())
    ]}}}.

-endif.
