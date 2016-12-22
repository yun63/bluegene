%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2010.11.08
%%% @doc 负责处理此gateway节点相关的消息,如广播.
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(gate_node).
-author("litaocheng@gmail.com").
-vsn('0.1').
-behaviour(gen_server).

-include("wg.hrl").
-include("const.hrl").
-include("common.hrl").
-include("gate_internal.hrl").

-export([start_link/1]).
-export([all_client/0, online_count/0, get_ip_port/0, is_started/0]).
-export([stop_server/0, stop_servers/0, health_check/0, health_check/1]).
-export([send/4, send_all/3, send_node/3, send_node/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).
                            
-record(state, {
        ip_port = []     % 监听的ip,端口列表
    }).

%% 定义名字
-define(START_NAME, {local, ?MODULE}).
-define(NAME, ?MODULE).

%% @doc 启动gate_node
start_link(IpPort) when is_list(IpPort) ->
    gen_server:start_link(?START_NAME, ?MODULE, IpPort, []).

%% @doc 获取所有的gate_client进程
all_client() ->
    lists:append(call(all_client)).

%% @doc 在线人数
online_count() ->
    lists:sum(call(online_count)).

%% @doc 查询节点监听的ip和端口
get_ip_port() ->
    lists:append(call(get_ip_port)).

%% @doc 其它节点,判断gateway是否启动
is_started() ->
    L = world_nodes:gateway_nodes(),
    case L of
        [] ->
            false;
        [Node|_] ->
            Pid = rpc:call(Node, erlang, whereis, [?MODULE]),
            Pid =/= undefined
    end.

%% @doc 停止所有gateway node,踢出所有玩家
%% 此操作比较耗时,设置较长的timeout
%% 1,tcp server拒绝新的连接
%% 2,所有的client,顺序停止
stop_server() ->
    call(stop_server, 120000),
    ok.
stop_servers() ->
    cast(killall_9),
    ok.
health_check() ->
    cast({health_check, {gate_conf_health, 1}}).
health_check({Tag, N}) ->
    cast({health_check, {Tag, N}}),
    ok.

%% @doc 向多个gate_client发送消息
send(_SendType, [], _MsgId, _Req) ->
    ok;
send(SendType, UIds, MsgId, Req) ->
    cast({send, SendType, UIds, MsgId, Req}),
    ok.

%% @doc 向所有的gate_client发送消息（本操作比较耗时，慎重使用）
send_all(SendType, MsgId, Req) ->
    cast({send_all, SendType, MsgId, Req}),
    ok.

%% @doc 本节点所有的gate_client发送消息
send_node(SendType, MsgId, Req) ->
    cast(node(), {send_all, SendType, MsgId, Req}),
    ok.

%% @doc 指定节点所有的gate_client发送消息
send_node(Node, SendType, MsgId, Req) ->
    cast(Node, {send_all, SendType, MsgId, Req}),
    ok.

%%
%% gen_server callbacks
%%
init(IpPort) ->    
    erlang:process_flag(trap_exit, true),
    erlang:process_flag(priority, high),
    {ok, #state{ip_port = IpPort}}.

handle_call(all_client, _From, State) ->
    ?INFO(?_U("查询所有的client"), []),
    Reply = gate_client_mgr:all(),
    {reply, Reply, State};
handle_call(online_count, _From, State) ->
    ?INFO(?_U("查询在线人数"), []),
    Reply = gate_client_mgr:count(),
    {reply, Reply, State};
handle_call(get_ip_port, _From, #state{ip_port = IpPort} = State) ->
    ?DEBUG(?_U("查询对应的ip和port"), []),
    {reply, IpPort, State};
handle_call(stop_server, _From, #state{ip_port = IpPort} = State) ->
    ?INFO(?_U("停止服务器"), []),
    [begin
        ok = wg_tcp_server:refuse_conn(Name, true)
    end || {Name, _, _} <- IpPort],
    [begin
        gate_client:stop_by_gateway(Client)
    end || Client <- gate_client_mgr:all_pid()],
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({send, SendType, UIds, MsgId, Req}, State) ->
    %?DEBUG(?_U("向指定玩家:~p发送消息:~p"), [UIds, MsgId]),
    ok = do_send(SendType, UIds, MsgId, Req),
    {noreply, State};
handle_cast({send_all, SendType, MsgId, Req}, State) ->
    %?DEBUG(?_U("向所有玩家发送消息:~p"), [MsgId]),
    ok = do_send_all(SendType, MsgId, Req),
    {noreply, State};
handle_cast(killall_9, #state{ip_port = IpPort} = State) ->
    [begin
        ok = wg_tcp_server:refuse_conn(Name, true)
    end || {Name, _, _} <- IpPort],
    [gen_fsm:send_all_state_event(Client, killall_9)
     || Client <- gate_client_mgr:all_pid()],
    {noreply, State};
handle_cast({health_check, {Tag, N}}, State) ->
    ?CONF_SET(Tag, N),
    [gen_fsm:send_all_state_event(Client, health_check)
     || Client <- gate_client_mgr:all_pid()],
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?DEBUG(?_U("gate_node结束:~p"), [_Reason]),
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
    
%%--------------
%% internal API
%%--------------

%% 调用
call(Req) ->
    call(Req, ?GEN_TIMEOUT).
call(Req, Timeout) ->
    [begin
        gen_server:call({?NAME, Node}, Req, Timeout)
    end || Node <- world_nodes:gateway_nodes()].

%% 异步调用
cast(Req) ->
    [begin
        gen_server:cast({?NAME, Node}, Req)
    end || Node <- world_nodes:gateway_nodes()].

cast(Node, Req) ->
    gen_server:cast({?NAME, Node}, Req).

%% 向指定client发送消息 
do_send(SendType, UIds, MsgId, Req) ->
    [begin
        case gate_client_mgr:get(Id) of
            ?NONE ->
                ok;
            Pid ->
                ok = gate_client:SendType(Pid, MsgId, Req)
        end
    end || Id <- UIds],
    ok.

%% 向所有client发送消息
do_send_all(SendType, MsgId, Req) ->
    [catch gate_client:SendType(Pid, MsgId, Req) || 
        Pid <- gate_client_mgr:all_pid()],
    ok.

%%------------
%% EUNIT test
%%------------

-ifdef(EUNIT).

-endif.
