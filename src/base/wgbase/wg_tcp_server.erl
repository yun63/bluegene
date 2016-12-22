%%%----------------------------------------------------------------------------
%%%
%%% @author : litaocheng
%%% @date:  2010.10.22
%%% @doc 
%%%  tcp socket server
%%%  其结构如下
%%%     tcp_server_sup
%%%          |
%%%      acceptor(name)
%%%     / | | | \ % monitor
%%% client_1 ... client_N
%%% @end
%%%
%%%----------------------------------------------------------------------------
-module(wg_tcp_server).
-behaviour(supervisor).
-vsn('0.1').
-include("wg_internal.hrl").

-export([start_link/5, start_link/6, start_link/1]).
-export([default_opts/0]).
-export([max_conn/1, current_conn/1, clients/1, refuse_conn/2]).
-export([init/1]).

-ifdef(EUNIT).
-export([dummy/1]).
-endif.

-define(TCP_DEFAULT_OPTS, 
    [
    {active, once},
    {delay_send, false},    % 当包过多时，可以修改为true
    {nodelay, true},
    {reuseaddr, true},
    {send_timeout, 5000},
    {send_timeout_close, true}
    %{recbuf, 81920},
    %{sndbuf, 81920}
    ]).

%% tcp server 选项
-record(server_option, {
        name,           % acceptor 名称
        type = binary,  % 数据类型为list or binary
        ip = any,
        port = 0,
        packet = 0,
        callback,       % TCP链接处理函数，需要创建一个新的process
        backlog = 512,
        max = 5000,     % 最大连接数
        opts = []       % 其他选项，参考inet:setopts/2
    }).

%% @doc 启动tcp listener sup
start_link(Name, IPAddress, Port, Packet, Callback) ->
    ?DEBUG("start_link the tcp server name:~p ip:~p port:~p", [Name, IPAddress, Port]),
    Options = [{name, Name}, {ip, IPAddress}, {port, Port}, 
        {packet, Packet}, {callback, Callback} | ?TCP_DEFAULT_OPTS],
    ServerOption = parse_options(Options),
    do_start_link(ServerOption).

%% @doc 启动tcp listener sup
start_link(Name, IPAddress, Port, Packet, Callback, Opts) ->
    ?DEBUG("start_link the tcp server name:~p ip:~p port:~p", [Name, IPAddress, Port]),
    Opts1 = ?TCP_DEFAULT_OPTS -- Opts,
    Options = [{name, Name}, {ip, IPAddress}, {port, Port}, 
        {packet, Packet}, {callback, Callback} | Opts ++ Opts1],
    ServerOption = parse_options(Options),
    do_start_link(ServerOption).

%% @doc 启动tcp server sup
%% Options 包含:
%% type, ip, port, packet, callback, backlog, max
%% 以及其他除packet之外的所有inet:setopts中定义关于tcp的选项
%% @end
start_link(Options) ->
    ServerOption = parse_options(Options),
    do_start_link(ServerOption).

%% @doc 返回默认选项
default_opts() ->
    ?TCP_DEFAULT_OPTS.

%% @doc 获取最大允许连接数
max_conn(Acceptor) ->
    wg_tcp_acceptor:max_conn(Acceptor).

%% @doc 获取当前连接数
current_conn(Acceptor) ->
    wg_tcp_acceptor:current_conn(Acceptor).

%% @doc 获取client连接列表
clients(Acceptor) ->
    wg_tcp_acceptor:clients(Acceptor).

%% @doc 拒绝接受新连接
refuse_conn(Server, Refuse) ->
    wg_tcp_acceptor:refuse_conn(Server, Refuse).

%% supervisor callback
init(ServerOption= #server_option{name = Name, callback = Callback, max = Max}) ->
    ?DEBUG("init the tcp_server sup", []),
    case catch start_listener(ServerOption) of
        {{error, _Reason}, _} ->
            exit(_Reason);
        {{ok, LSock}, CommOpts} ->
            ?DEBUG("listening port ~p now", [ServerOption#server_option.port]),
            Strategy = {one_for_one, 10, 10},
            Mod = {tcp_acceptor, {wg_tcp_acceptor, start_link, [Name, LSock, CommOpts, Callback, Max]},
                permanent, brutal_kill, worker, [tcp_acceptor]},
            {ok, {Strategy, [Mod]}}
    end.

%%%
%%% internal API
%%%

%% 启动supervisor
do_start_link(Option = #server_option{}) ->
    %?DEBUG("do_start_link option ~p", [Option]),
    supervisor:start_link(?MODULE, Option).

%% 解析选项
%% type, ip, port, packet, callback, backlog, max
parse_options(Options) ->
    parse_options(Options, #server_option{}).

parse_options([{name, Name} | R], State) when is_atom(Name) ->
    parse_options(R, State#server_option{name = Name});
parse_options([{name, Name} | R], State) when is_list(Name) ->
    parse_options(R, State#server_option{name = list_to_atom(Name)});
parse_options([{type, Type} | R], State) when Type =:= binary; Type =:= list ->
    parse_options(R, State#server_option{type = Type});
parse_options([{ip, Ip} | R], State) ->
    ParsedIp =
    case Ip of
        any ->
            any;
        Ip when is_tuple(Ip) ->
            Ip;
        Ip when is_list(Ip) ->
            {ok, IpTuple} = inet_parse:address(Ip),
            IpTuple
    end,
    parse_options(R, State#server_option{ip = ParsedIp});
parse_options([{port, Port} | R], State) when is_integer(Port) ->
    parse_options(R, State#server_option{port = Port});
parse_options([{port, Port} | R], State) when is_list(Port) ->
    parse_options(R, State#server_option{port = list_to_integer(Port)});
parse_options([{packet, Packet} | R], State) ->
    parse_options(R, State#server_option{packet = Packet});
parse_options([{callback, Cb} | R], State) ->
    true = check_callback(Cb),
    parse_options(R, State#server_option{callback = Cb});
parse_options([{backlog, Backlog} | R], State) when is_integer(Backlog) ->
    parse_options(R, State#server_option{backlog = Backlog});
parse_options([{max, Max} | R], State) when is_integer(Max) ->
    parse_options(R, State#server_option{max = Max});
parse_options([Other | R], State = #server_option{opts = Opts}) ->
    parse_options(R, State#server_option{opts = [Other | Opts]});
parse_options([], State) ->
    State.

%% check callback
check_callback({M, F, A}) ->
    wg_util:is_exported({M, F, length(A)+1});
check_callback(Fun) when is_function(Fun, 1) -> true.

%% 检查是否支持ipv6
ipv6_supported() ->
    case (catch inet:getaddr("localhost", inet6)) of
        {ok, _Addr} ->
            true;
        {error, _} ->
            false
    end.

%% 监听端口
start_listener(#server_option{
        type = Type,
        ip = Ip,
        port = Port,
        packet = Packet,
        backlog = Backlog,
        opts = OtherOpts}) ->
    Inet =
    case Ip of
        any ->
            case ipv6_supported() of
                true ->
                    inet6; 
                false ->
                    inet
            end;
        {_, _, _, _} -> % IPV4
            inet;
        {_, _, _, _, _, _} -> %IPV6
            inet6
    end,
    CommOpts = [{packet, Packet} | OtherOpts],
    ListenOpts = [Inet, Type, {ip, Ip}, {backlog, Backlog} | CommOpts],
    ?DEBUG("listen options are ~p", [ListenOpts]),
    {gen_tcp:listen(Port, ListenOpts), CommOpts}.

%% 获取accetpor进程
%get_acceptor(Server) ->
%    [{_Id, Child, _Type, _Modules}] = supervisor:which_children(Server),
%    {ok, Child}.

%%
%% EUNIT test
%% 
-ifdef(EUNIT).

dummy(_Sock) ->
    ok.

check_callback_test_() ->
    [
    ?_assertEqual(true, check_callback({?MODULE, dummy, []})),
    ?_assertEqual(false, check_callback({?MODULE, dummy2, []})),
    ?_assertEqual(true, check_callback(fun(_Sock) -> ok end))
    ].

parse_options_test() ->
    % ip
    ?assertEqual(#server_option{ip = any}, 
        parse_options([{ip, any}])),
    ?assertEqual(#server_option{ip = {10,0,0,1}}, 
        parse_options([{ip, "10.0.0.1"}])),
    ?assertEqual(#server_option{ip = {10,0,0,1}}, 
        parse_options([{ip, {10,0,0,1}}])),

    % all
    ?assertEqual(#server_option{type = binary, ip = {10,0,0,1},
            port = 1000, packet = raw, callback = {?MODULE, dummy, []}, 
            backlog = 511, max = 5000},
        parse_options([{type, binary}, {ip, "10.0.0.1"},
                {port, 1000}, {packet, raw}, {callback, {?MODULE, dummy, []}},
                {backlog, 511}, {max, 5000}])),

    ?assertEqual(#server_option{opts = [{other_opt2, val2}, {other_opt1, val1}]},
        parse_options([{other_opt1, val1}, {other_opt2, val2}])),
    ok.

-endif.
