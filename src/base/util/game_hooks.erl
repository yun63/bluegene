%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2010.11.20
%%% @doc hooks管理，程序中在很多关键点都设置了hook，当其执行时会执行hook
%%% 中操作，从而减少逻辑复杂性
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(game_hooks).
-author("litaocheng@gmail.com").
-vsn('0.1').

-behaviour(gen_server).

%% External exports
-export([start_link/0,
	 add/3,
	 add/4,
	 add_dist/5,
     delete/2,
	 delete/3,
	 delete/4,
	 delete_dist/5,
	 run/2,
	 run_fold/3,
     fun_list/1,
     server_name/0
    ]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 code_change/3,
	 handle_info/2,
	 terminate/2]).

-include("wg.hrl").

%% Timeout of 5 seconds in calls to distributed hooks
-define(DIST_HOOK_TIMEOUT, 5000).
%% 表的名字
-define(TABLE, '$game_hooks_tab').
%% 服务名称
-define(SERVER, ?MODULE).

-record(state, {}).

%% @doc 启动hooks
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec (Function::function(), Seq::integer()) -> ok
%% @doc See add/4.
add(Hook, Function, Seq) when is_function(Function) ->
    add(Hook, undefined, Function, Seq);
add(Hook, Module, Function) when is_atom(Module), is_atom(Function) ->
    add(Hook, Module, Function, 0).

%% @spec (Hook::atom(), Module::atom(), Function::atom(), Seq::integer()) -> ok
%% @doc 为某个hook添加一个处理Module:Function
%% The integer sequence is used to sort the calls: low number is called before high number.
add(Hook, Module, Function, Seq) when is_integer(Seq), Seq >= 0 ->
    call({add, Hook, Module, Function, Seq}).

%% @doc 为某个hook添加处理，其调用在Node进行
add_dist(Hook, Node, Module, Function, Seq) ->
    call({add, Hook, Node, Module, Function, Seq}).

%% @spec (Hook::atom(), Function::function()) -> ok
%% @doc See del/4.
delete(Hook, Function) when is_function(Function) ->
    delete(Hook, Function, 0).

%% @doc See del/4.
delete(Hook, Function, Seq) when is_function(Function), is_integer(Seq) ->
    delete(Hook, undefined, Function, Seq);
delete(Hook, Module, Function) when is_atom(Module), is_atom(Function) ->
    delete(Hook, Module, Function, 0).

%% @spec (Hook::atom(), Module::atom(), Function::atom(), Seq::integer()) -> ok
%% @doc 从hook中删除一个处理
%% It is important to indicate exactly the same information than when the call was added.
delete(Hook, Module, Function, Seq) when is_integer(Seq), Seq >= 0 ->
    call({delete, Hook, Module, Function, Seq}).

%% @doc 删除对某个hook的处理
delete_dist(Hook, Node, Module, Function, Seq) when is_integer(Seq), Seq >= 0 ->
    delete_dist(Hook, Node, Module, Function, Seq).

%% @spec (Hook::atom(), Args) -> ok
%% @doc 顺序执行hook中的所有函数，不关注每个函数的返回值
%% 如果某个函数返回stop，则剩余的处理函数不会被调用
run(Hook, Args) ->
    case ets:lookup(?TABLE, Hook) of
        [{_, Ls}] ->
            run1(Ls, Hook, Args);
        [] ->
            ok
    end.

%% @spec (Hook::atom(), Val, Args) -> Val | stop | {stop, NewVal}
%% @doc 执行hook中所有函数，函数的调用结果会作为下一个调用的参数。
%% 如果函数返回'stop'，则调用停止返回'stop'
%% 如果函数返回{'stop', NewVal}，则调用停止返回{stop, NewVal}
run_fold(Hook, Val, Args) ->
    case ets:lookup(?TABLE, Hook) of
        [{_, Ls}] ->
            run_fold1(Ls, Hook, Val, Args);
        [] ->
            Val
    end.

%% @doc 获取某个hook下的函数列表
fun_list(Hook) ->
    case ets:lookup(?TABLE, Hook) of
        [{_, Ls}] ->
            Ls;
        [] ->
            []
    end.

%% @doc 获取gen_server名称
server_name() ->
    ?SERVER.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------
-spec init(any()) -> {ok, #state{}}.
init(_Args) ->
    ?TABLE = ets:new(?TABLE, [set, protected, named_table, 
            {keypos, 1},
            ?ETS_CONCURRENCY
            ]),
    {ok, #state{}}.

handle_call({add, Hook, Module, Function, Seq}, _From, State) ->
    Elem = {Seq, Module, Function},
    Reply = do_add(Hook, Elem),
    {reply, Reply, State};
handle_call({add, Hook, Node, Module, Function, Seq}, _From, State) ->
    Elem = {Seq, Node, Module, Function},
    Reply = do_add(Hook, Elem),
    {reply, Reply, State};
handle_call({delete, Hook, Module, Function, Seq}, _From, State) ->
    Elem = {Seq, Module, Function},
    Reply = do_delete(Hook, Elem),
    {reply, Reply, State};
handle_call({delete, Hook, Node, Module, Function, Seq}, _From, State) ->
    Elem = {Seq, Node, Module, Function},
    Reply = do_delete(Hook, Elem),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?DEBUG("terminate reason:~p", [_Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------
%% Internal functions
%%--------------------

call(Req) ->
    gen_server:call(?SERVER, Req).

%% 某个hook中添加处理
do_add(Hook, Elem) ->
    case ets:lookup(?TABLE, Hook) of
		[{_, Ls}] ->
		    case lists:member(Elem, Ls) of
                true ->
                    ok;
                false ->
                    NewLs = lists:merge(Ls, [Elem]),
                    ets:insert(?TABLE, {Hook, NewLs}),
                    ok
		    end;
		[] ->
		    NewLs = [Elem],
		    ets:insert(?TABLE, {Hook, NewLs}),
		    ok
    end.

%% 删除hook中的处理
do_delete(Hook, Elem) ->
    case ets:lookup(?TABLE, Hook) of
		[{_, Ls}] ->
		    NewLs = lists:delete(Elem, Ls),
		    ets:insert(?TABLE, {Hook, NewLs}),
		    ok;
		[] ->
		    ok
    end.

%% 顺序执行hook中函数
run1([], _Hook, _Args) ->
    ok;
run1([{_Seq, Node, Module, Function} | Ls], Hook, Args) ->
    case rpc:call(Node, Module, Function, Args, ?DIST_HOOK_TIMEOUT) of
        timeout ->
            ?ERROR("Timeout on RPC to ~p~nrunning hook: ~p",
                   [Node, {Hook, Args}]),
            run1(Ls, Hook, Args);
        {badrpc, Reason} ->
            ?ERROR("Bad RPC error to ~p: ~p~nrunning hook: ~p",
                   [Node, Reason, {Hook, Args}]),
            run1(Ls, Hook, Args);
        stop ->
            ?INFO("~nThe process ~p in node ~p ran a hook in node ~p.~n"
                  "Stop.", [self(), node(), Node]), % debug code
            ok;
        Res ->
            ?INFO("~nThe process ~p in node ~p ran a hook in node ~p.~n"
                  "The response is:~n~s", [self(), node(), Node, Res]), % debug code
            run1(Ls, Hook, Args)
    end;
run1([{_Seq, Module, Function} | Ls], Hook, Args) ->
    Res = 
    if 
        is_function(Function) ->
            catch apply(Function, Args);
        true ->
            catch apply(Module, Function, Args)
    end,
    case Res of
        {'EXIT', Reason} ->
            ?ERROR(?_U("执行hook:~p mod:~p fun:~p 参数:~p错误:~p"),
                   [Hook, Module, Function, Args, Reason]),
            run1(Ls, Hook, Args);
        stop ->
            ok;
        {error, Reason} ->
            ?WARN(?_U("执行hook返回值错误:~p"), [Reason]),
            run1(Ls, Hook, Args);
        _Other ->
            run1(Ls, Hook, Args)
    end.

%% 执行hook中函数
run_fold1([], _Hook, Val, _Args) ->
    Val;
run_fold1([{_Seq, Node, Module, Function} | Ls], Hook, Val, Args) ->
    case rpc:call(Node, Module, Function, [Val | Args], ?DIST_HOOK_TIMEOUT) of
        {badrpc, Reason} ->
            ?ERROR("Bad RPC error to ~p: ~p~nrunning hook: ~p",
                   [Node, Reason, {Hook, Args}]),
            run_fold1(Ls, Hook, Val, Args);
        timeout ->
            ?ERROR("Timeout on RPC to ~p~nrunning hook: ~p",
                   [Node, {Hook, Args}]),
            run_fold1(Ls, Hook, Val, Args);
        stop ->
            stop;
        {stop, NewVal} ->
            ?INFO("~nThe process ~p in node ~p ran a hook in node ~p.~n"
                  "Stop, and the NewVal is:~n~p", [self(), node(), Node, NewVal]), % debug code
            {stop, NewVal};
        NewVal ->
            ?INFO("~nThe process ~p in node ~p ran a hook in node ~p.~n"
                  "The NewVal is:~n~p", [self(), node(), Node, NewVal]), % debug code
            run_fold1(Ls, Hook, NewVal, Args)
    end;
run_fold1([{_Seq, Module, Function} | Ls], Hook, Val, Args) ->
    {_CostTime, Res} =
    if
        is_function(Function) ->
                catch timer:tc(Function, [Val | Args]);
	    true ->
                catch timer:tc(Module, Function, [Val | Args])
    end,
    % 对超过200ms的调用进行记录
%    ?IF(_CostTime > 200000,
%       ?WARN("game_hooks cost ~p for ~p", [_CostTime, [Module, Function, Args]]),
%       ok),
    case Res of
        {error, Code} ->
            ?ERROR("error:~p~nrunning hook: ~p",
                   [Code, {Hook, Args}]),
            error({code, Code});
        {'EXIT', Reason} ->
            ?ERROR("~p~nrunning hook: ~p",
                   [Reason, {Hook, Args}]),
            error(Reason);
        stop ->
            stop;
        {stop, NewVal} ->
            {stop, NewVal};
        NewVal ->
            run_fold1(Ls, Hook, NewVal, Args)
    end.

%%--------------
%% EUNIT TEST
%%--------------
-ifdef(EUNIT).

-define(TEST_HOOK1, "test_hook1").
-define(TEST_HOOK2, "test_hook2").
-define(TEST_HOOK3, "test_hook3").
-define(TEST_HOOK4, "test_hook4").
-define(TEST_HOOK5, "test_hook5").

main_test_() ->
    {spawn, {inorder, {setup,
    fun() -> game_hooks:start_link() end,
    fun({ok, Pid}) -> erlang:exit(Pid, kill) end,
    [
    ?_assertEqual([], game_hooks:fun_list(?TEST_HOOK1)),
    ?_assertEqual(ok, game_hooks:add(?TEST_HOOK1, fun() -> ok end, 1)),
    ?_assertMatch([{1, _, _}], game_hooks:fun_list(?TEST_HOOK1)),

    ?_assertEqual(ok, game_hooks:add(?TEST_HOOK1, fun(_) -> ok end, 2)),
    ?_assertMatch([{1, _, _}, {2, _, _}], game_hooks:fun_list(?TEST_HOOK1)),

    ?_assertEqual(ok, game_hooks:add(?TEST_HOOK2, ?MODULE, test, 3)),
    ?_assertEqual([{3, ?MODULE, test}], game_hooks:fun_list(?TEST_HOOK2)),
    ?_assertEqual(ok, game_hooks:add(?TEST_HOOK2, ?MODULE, first, 1)),
    ?_assertEqual([{1, ?MODULE, first}, {3, ?MODULE, test}], 
        game_hooks:fun_list(?TEST_HOOK2)),
    % 删除
    ?_assertEqual(ok, game_hooks:delete(?TEST_HOOK2, ?MODULE, test, 3)),
    ?_assertEqual([{1, ?MODULE, first}], game_hooks:fun_list(?TEST_HOOK2)),
    ?_assertEqual(ok, game_hooks:delete(?TEST_HOOK2, ?MODULE, first, 1)),
    ?_assertEqual([], game_hooks:fun_list(?TEST_HOOK2)),

    % run
    fun() ->
        put(k1, 1),
        put(k2, 2),
        ok = game_hooks:add(?TEST_HOOK3, fun() -> erase(k1) end, 1),
        ok = game_hooks:add(?TEST_HOOK3, fun() -> erase(k2) end, 2),
        ?assertEqual(ok, game_hooks:run(?TEST_HOOK3, [])),
        ?assertEqual(undefined, get(k1)),
        ?assertEqual(undefined, get(k2)),

        put(k1, 1),
        put(k2, 2),
        ok = game_hooks:add(?TEST_HOOK4, 
            fun() -> erase(k1), stop end, 1),
        ok = game_hooks:add(?TEST_HOOK4,
            fun() -> erase(k2) end, 2),
        ?assertEqual(ok, game_hooks:run(?TEST_HOOK4, [])),
        ?assertEqual(undefined, get(k1)),
        ?assertEqual(2, get(k2))
    end,
    fun() ->
        F1 = fun(N) -> N+1 end,
        ok = game_hooks:add(?TEST_HOOK5, F1, 1),
        ok = game_hooks:add(?TEST_HOOK5, F1, 2),
        ?assertEqual(3, game_hooks:run_fold(?TEST_HOOK5, 1, [])),

        F2 = fun(N) -> N*2 end,
        ok = game_hooks:add(?TEST_HOOK5, F2, 0),
        ?assertEqual(4, game_hooks:run_fold(?TEST_HOOK5, 1, [])),
        
        F3 = fun(N) when N >= 5 -> {stop, 5} end,
        ok = game_hooks:add(?TEST_HOOK5, F3, 0),
        ?assertEqual({stop, 5}, game_hooks:run_fold(?TEST_HOOK5, 10, []))
    end
    ]}}}.

-endif.
