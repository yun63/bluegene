%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2010.10.27
%%% @doc 使用FSM处理TCP链接，包括接收客户端请求，向客户端发送数据
%%% 所有的数据均经此process进行处理
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(gate_client).
-author("litaocheng@gmail.com").
-vsn('0.1').
-behaviour(gen_fsm).
-include("wg.hrl").
-include("gate.hrl").
-include("errno.hrl").
-include("const.hrl").
-include("account.hrl").
-include("common.hrl").
-include("gate_internal.hrl").
-include("proto/proto.hrl").
-include("proto/proto_acc.hrl").

-export([start_link/1, stop_by_player/1, stop_by_gateway/1]).
-export([send_push/3, send_merge/3, peer/1, pid/1, socket/1]).
-export([start_switch_node/1, finish_switch_node/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
                            terminate/3, code_change/4]).
%% FSM状态
%% 'WAIT_LOGIN' :等待用户登录
%% 'WAIT_PLAY'  :等待用户进入游戏（处理关于角色的请求）
%% 'PLAYING'    :玩家正在玩游戏（所有的请求转发给player)

-export(['WAIT_LOGIN'/2, 'WAIT_PLAY'/2, 'PLAYING'/2, 'PLAYING'/3]).

-import(procdict_list, [get_list/1, set_list/2, erase_list/1, list_add/2, list_delete/2,
        list_keydelete/3, list_keyfind/3, list_keyupdate/4]).

%%
%% 宏定义
%%

-define(WAIT_LOGIN, 'WAIT_LOGIN').
-define(WAIT_PLAY, 'WAIT_PLAY').
-define(PLAYING, 'PLAYING').

%% 心跳间隔
-define(HEART_TIME, 20000).
%% 心跳次数
-define(HEART_TIMEOUT_MAX, 3000000000000000). % 临时去除心跳检测
%% 流畅检测最大超标次数
-define(TRAFFIC_OVER_MAX, 3).

%% spawn opt
-define(SPAWN_OPT, [{min_heap_size, 10240},
        {min_bin_vheap_size, 46368},
        {fullsweep_after, 6000}]).

%% 名称
-define(NAME(Id), game_misc:client_name(Id)).

%% @doc 处理一个新的链接
start_link(Sock) ->
    % 此时client不注册名字，当登录成功后注册
    gen_fsm:start_link(?MODULE, Sock, [{spawn_opt, ?SPAWN_OPT}]).

%% @doc 结束连接
stop_by_player(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, {stop, player}).

%% @doc 结束,gateway调用
stop_by_gateway(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, {stop, gateway}).

%% @doc 立刻发送消息
send_push(Id, MsgId, Record) ->
    send_event(Id, {push, MsgId, Record}).

%% @doc 发送消息
send_merge(Id, MsgId, Record) ->
    send_event(Id, {merge, MsgId, Record}).

%% @doc 获取对应的socket(调试使用)
socket(Id) ->
    call(Id, get_socket).

%% @doc 获取对应的ip
peer(Id) ->
    Sock = call(Id, get_socket),
    wg_util:peer_str(Sock).

%% @doc 获取对应的进程id(调试使用)
pid(Id) when is_integer(Id) ->
    gate_client_mgr:get(Id).

%% @doc 通知gate_client玩家开始切换节点
start_switch_node(Id) ->
    call_all_state(Id, start_switch_node).

%% @doc 通知gate_client完成切换节点
finish_switch_node(Id, PlayerServerPid) ->
    call_all_state(Id, {finish_switch_node, PlayerServerPid}).

%%-------------------
%% gen_fsm callbacks
%%-------------------
init(Sock) ->    
    % *注意* 此刻本进程还不是Sock的controlling process
    erlang:process_flag(trap_exit, true),
    random:seed(now()),
    try
        Ip = wg_util:peer(Sock),
        case gm_ctrl:is_ip_ban_timein_exists(Ip) of
             true ->
                 ?WARN(?_U("玩家尝试登录,ip:~p已被封锁"), [Ip]),
                 {stop, normal};
             false ->
                %start_heart_timer(),
                % dev环境不起流量检测
                ?IF("dev" =/= ?CONF(platform, "dev"), start_traffic_timer(), ok),
                start_flush_prepare_msg_timer(),
                IpStr = inet_parse:ntoa(Ip),
                State = #client_state{
                    sock = Sock, 
                    ip = IpStr,
                    health = ?CONF(gate_conf_health, 0),
                    log = ?CONF(gate_conf_log, 0)
                },
                {ok, ?WAIT_LOGIN, State}
        end
    catch
        throw:{error, Reason} ->
            ?WARN(?_U("gate client初始化过程中,失败:~p"), [Reason]),
            {stop, normal}
    end.

handle_event(killall_9, _StateName, State) ->
    {stop, normal, State};
handle_event(health_check, StateName, State) ->
    State2 = State#client_state{
        health = ?CONF(gate_conf_health, 0),
        log = ?CONF(gate_conf_log, 0)
    },
    {next_state, StateName, State2};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(start_switch_node, _From, ?PLAYING, 
    #client_state{id = Id, player_pid = PlayerPid} = State) ->
    ?DEBUG(?_U("client:~p对应逻辑进程开始节点切换"),[Id]),
    erlang:unlink(PlayerPid),
    State2 = State#client_state{player_pid = ?NONE,
        switch_node = true
    },
    {reply, ok, ?PLAYING, State2};
handle_sync_event({finish_switch_node, PlayerPid}, _From, ?PLAYING, 
        #client_state{id = Id} = State) ->
    ?DEBUG(?_U("client:~p完成节点切换"),[Id]),
    erlang:link(PlayerPid),
    State2 = State#client_state{player_pid = PlayerPid,
        switch_node = false
    },
    {reply, ok, ?PLAYING, State2};
handle_sync_event({stop, By}, _From, _StateName, State) ->
    ?DEBUG(?_U("client进程停止"),[]),
    case By of
        gateway ->
            #client_state{player_pid = Player} = State,
            ?IF(is_pid(Player), player_server:sync_stop(Player), ok);
        player ->
            ok
    end,
    {stop, normal, ok, State};
handle_sync_event(_Event, _From, StateName, State) ->
    {reply, handled, StateName, State}.

%% 处理TCP相关
handle_info({tcp, Sock, ?FLASH_POLICY_REQ}, StateName = ?WAIT_LOGIN, State) ->
    % 收到flash安全沙箱请求
    %?DEBUG(?_U("收到安全沙箱请求....")),
    do_send_data(Sock, ?FLASH_POLICY_ACK, StateName, State),
    {next_state, StateName, State};
handle_info({tcp, Sock, Data}, StateName = ?WAIT_LOGIN,
        State = #client_state{sock = Sock, part = Part}) ->
    ?IF(Part =/= <<>>,
        ?WARN(?_U("状态~p收到数据包:~p~n残存数据~p"), [StateName, Data, Part]),
        ok),
    TwgTag = ?CONF(tgw_header, "tgw_l7_forward"),
    case Data of
        TwgTag ->
            ?DEBUG("tgw_header received"),
            do_active(Sock),
            {next_state, StateName, State};
        _Any ->
            % 处理游戏协议
            case catch do_handle_proto(Data, State) of
                {ok, {_MsgId, _MF, {debug_ask_log_c2s, _Num1, _Num2}}, State0} ->
                    ?WARN(?_U("状态~p收到数据包:~p"), [StateName, Data]),
                    do_active(Sock),
                    {next_state, StateName,  State0};
                {ok, {MsgId, MF, Req}, State0} ->
                    % 处理登录
                    {ok, StateName2, State2} = do_login_validate(MF, MsgId, Req, StateName, State0),
                    do_active(Sock),
                    {next_state, StateName2, State2};
                {continue, State0} ->
                    do_active(Sock),
                    {next_state, StateName,  State0}
            end
    end;
handle_info({tcp, Sock, Data}, StateName = ?WAIT_PLAY, 
        State = #client_state{sock = Sock, part = Part0}) ->
    ?IF(Part0 =/= <<>>,
        ?WARN(?_U("状态~p收到数据包:~p~n残存数据~p"), [StateName, Data, Part0]),
        ok),
    % 处理协议
    case catch do_handle_proto(Data, State) of
        {ok, {_MsgId, _MF, {debug_ask_log_c2s, Num1, Num2}}, State0} ->
            game_log:client_ask_log_pre_login(State#client_state.accname, Num1, Num2),
            Part = State0#client_state.part,
            ?IF(Part =:= <<>>,
                begin
                    do_active(Sock),
                    {next_state, StateName, State0}
                end,
                handle_info({tcp, Sock, Part}, StateName, State0#client_state{part = <<>>}));
        {ok, {MsgId, MF, Req}, State0} ->
            % 处理帐号角色相关操作
            case catch do_acc_process(MF, MsgId, Req, StateName, State0) of
                {ok, StateName2, State2} ->
                    Part = State2#client_state.part,
                    ?IF(Part =:= <<>> orelse StateName =/= StateName2,
                        begin
                            do_active(Sock),
                            {next_state, StateName2, State2}
                        end,
                        handle_info({tcp, Sock, Part}, StateName, State2#client_state{part = <<>>}));
                {stop, Reason} ->
                    {stop, Reason, State}
            end;
        {continue, State0} ->
            do_active(Sock),
            {next_state, StateName, State0}
    end;
handle_info({tcp, Sock, Data}, StateName = ?PLAYING, 
        State = #client_state{id = _Id, sock = Sock}) ->
    %?DEBUG(?_U("从玩家:~p 接收数据:~p"),
    %[?IF(_Id =:= undefined, "unknown", _Id), Data]),
    case catch do_process_proto_loop(Data, StateName, State) of
        {ok, State2} ->
            do_active(Sock),
            {next_state, StateName, State2};
        {continue, State0} ->
            do_active(Sock),
            {next_state, StateName, State0}
    end;
handle_info({tcp_closed, Sock}, StateName, State) ->
    when_sock_closed(Sock, StateName, State),
    {stop, normal, State};
handle_info({tcp_error, Sock, Reason}, StateName, State) ->
    ok = when_sock_error(Sock, Reason, StateName, State),
    {stop, normal, State};
handle_info({timeout, _HeartRef, heart_check}, StateName, 
        State = #client_state{}) ->
    do_heart_check(StateName, State);
handle_info({timeout, _Ref, traffic_check}, StateName, State) ->
    case catch do_traffic_check(State) of
        ok ->
            {next_state, StateName, State};
        {error, Code} ->
            ?WARN(?_U("玩家流量检测异常被踢出,Code:~p"), [Code]),
            {stop, normal, State}
    end;
handle_info(flush_prepare_msg, StateName, State) ->
    do_flush_prepare_msg(StateName, State),
    start_flush_prepare_msg_timer(),
    {next_state, StateName, State};
handle_info({'EXIT', _Pid, _Reason}, _StateName, State) ->
    ?ERROR(?_U("玩家进程:~p退出, reason: ~p，关闭socket,退出"),
           [_Pid, _Reason]),
    gen_tcp:close(State#client_state.sock),
    case _Reason of
        normal ->
            ok;
        _  ->
            Str = io_lib:format("~w", [_Reason]),
            ok = game_log:gateway_crash(State#client_state.accname, State#client_state.ip, Str)
    end,
    {stop, normal, State};

handle_info({inet_reply, _, ok}, StateName, State) ->
    {next_state, StateName, State};
handle_info({inet_reply, Sock, Status}, StateName, State) ->
    ok = when_sock_error(Sock, Status, StateName, State),
    {stop, normal, State};
handle_info(_Info, StateName, State) ->
    ?ERROR("recv unknown msg:~p", [_Info]),
    {next_state, StateName, State}.

%% 大部分的错误处理都在terminate中完成，此时会发送给
%% 客户端一条通知，随后结束进程
terminate(_Reason, _StateName, #client_state{id = Id}) ->
    ?IF(_Reason =:= normal, ok,
        ?ERROR("玩家:~p进程结束:~p",
            [?IF(Id =:= undefined, "unknown", Id), _Reason])),
    % 从gate_client_mgr删除
    ?IF(Id =:= undefined, ok,
        ok = gate_client_mgr:delete(Id)),
    ok.

code_change(_Old, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% FSM
'WAIT_LOGIN'(_Event, State) ->
    ?WARN(?_U("收到消息:~p in WAIT_LOGIN"), [_Event]),
    {next_state, ?WAIT_LOGIN, State}.

'WAIT_PLAY'(_Event, State) ->
    ?DEBUG(?_U("收到消息:~p in WAIT_PLAY"), [_Event]),
    {next_state, ?WAIT_PLAY, State}.

'PLAYING'({push, MsgId, Record}, State) ->
    % 向Client发送数据
    ok = do_send_msg({MsgId, Record}, ?PLAYING, State),
    {next_state, ?PLAYING, State};
'PLAYING'({merge, MsgId, Record}, State) ->
    % 向Client发送数据
    ok = do_send_msg_merge({MsgId, Record}, ?PLAYING, State),
    {next_state, ?PLAYING, State}.

'PLAYING'(get_socket, _From, #client_state{sock = Sock} = State) ->
    {reply, Sock, ?PLAYING, State}.

%%--------------
%% internal API
%%--------------

%% 请求
call(Id, Req) when is_integer(Id) ->
    gen_fsm:sync_send_event(?NAME(Id), Req);
call(Id, Req) when is_pid(Id) ->
    gen_fsm:sync_send_event(Id, Req).

%% 请求所有的state
call_all_state(Id, Req) when is_integer(Id) ->
    gen_fsm:sync_send_all_state_event(?NAME(Id), Req);
call_all_state(Id, Req) when is_pid(Id) ->
    gen_fsm:sync_send_all_state_event(Id, Req).

%% 发送事件
send_event(Id, Req) when is_integer(Id) ->
    % 获取所有的gateway节点
    L = world_nodes:gateway_nodes(),
    %?WARN(?_U("******向client:~p(~p)发送消息:~w"), [Id, L, Req]),
    % gen_fsm没有catch!
    [catch gen_fsm:send_event({?NAME(Id), Node}, Req)
        || Node <- L],
    ok;
send_event(Pid, Req) when is_pid(Pid) ->
    gen_fsm:send_event(Pid, Req).

%% 循环处理协议
do_process_proto_loop(<<>>, _StateName, State) ->
    {ok, State};
do_process_proto_loop(Data, StateName, State) ->
    {ok, {MsgId, MF, Req}, #client_state{part = Part} = State0} = do_handle_proto(Data, State),
    % 处理游戏中的交互
    {ok, State2} = do_process(MF, MsgId, Req, StateName, State0),
    do_process_proto_loop(Part, StateName, State2#client_state{part = <<>>}).

%% 处理协议
do_handle_proto(Data, #client_state{part = <<>>} = _State) ->
    do_handle_proto1(Data, _State);
do_handle_proto(Data, #client_state{part = Part} = _State) ->
    do_handle_proto1(<<Part/bytes, Data/bytes>>, _State#client_state{part = <<>>}).

do_handle_proto1(<<Len:16, Data:Len/bytes, _Rest/bytes>>, #client_state{id = _Id, log = _Log} = _State) ->
    {ok, MsgId, MF, Req} = proto:de_packet(Data),
    ?IF(_Log =:= 0,
        ok,
        ?IF2(element(1, Req) =/= acc_servertime_c2s, ?DEBUG("gate ~p receive msg ~p~n~p", [_Id, MsgId, Req]))),
    {ok, {MsgId, MF, Req}, _State#client_state{part = _Rest}};
do_handle_proto1(Data, #client_state{part = Part} = State) ->
    throw({continue, State#client_state{part = <<Part/bytes, Data/bytes>>}}).

%% 登录验证
do_login_validate({mod_acc, login}, MsgId, Req, StateName, State) ->
    try
        {AccName, Sid, Cm, OpenKey, Platform, Other} =
        case game_partner:auth(Req) of
            {ok, A, S, C} ->
                {A, S, C, <<>>, <<>>, []};
            {ok, A, S, C, K, P} ->
                {A, S, C, K, P, []};
            {ok, A, S, C, K, P, O} ->
                {A, S, C, K, P, O};
            {error, Code} ->
                throw({error, Code})
        end,
        case ?CONF(gateway_status, open) of
            open ->
                ok;
            testing ->
                ?IF(lists:member(AccName, ?CONF(testers, [])),
                    ok,
                    throw({error, ?E_AUTH}));
            _ ->
                throw({error, ?E_AUTH})
        end,
        ?DEBUG(?_U("玩家:~p登录验证成功"), [AccName]),
        ok = game_log:validate(AccName, Sid),
        case db_account:is_exist(AccName, Sid) of
            true ->
                State2 = State;
            false ->
                ok = db_account:create(AccName, Sid, State#client_state.ip),
                State2 = State#client_state{first_enter = true}
        end,
        ok = send_login_s2c(MsgId, ?E_OK, StateName, State2),
        State3 = State2#client_state{
            accname = AccName,
            sid = Sid,
            cm = Cm,
            open_key = OpenKey,
            platform = Platform,
            other = Other,
            connect_time = wg_util:now_sec()
        },
        {ok, ?WAIT_PLAY, State3}
    catch
        _:{error, Code2} ->
            ok = send_login_s2c(MsgId, Code2, StateName, State),
            {ok, ?WAIT_LOGIN, State}
    end;
%% 游客登录验证
do_login_validate({mod_acc, guest_login}, MsgId, _Req, StateName, State) ->
    game_partner:auth_guest(),
    % 游客暂时不支持，没时间做！！！
    Sid = 0,
    {ok, Id, Career, Sex, AccName} = gate_acc:make_random_user(State),
    ok = send_guest_s2c(MsgId, ?E_OK, StateName, State),
    ok = send_guest_role_s2c(Id, Career, Sex, StateName, State),
    {ok, ?WAIT_PLAY, State#client_state{accname = AccName, sid = Sid, cm = 0, connect_time = wg_util:now_sec()}};
do_login_validate(_, _MsgId, _Req, _StateName, State) ->
    ?WARN(?_U("等待登录时收到非法请求:~p"), [_Req]),
    {ok, ?WAIT_LOGIN, State}.


%% 帐号角色相关操作
%% 玩家进入游戏
do_acc_process({mod_acc, enter}, MsgId, _Req, StateName, State = #client_state{}) ->
    #client_state{
        accname = AccName,
        sid = Sid,
        cm = Cm,
        open_key = OpenKey,
        platform = Platform,
        other = Other,
        ip = Ip,
        first_enter = FirstEnter
    } = State,
    {ok, Id} = db_account:get_player_id(AccName, Sid),
    case gm_ctrl:is_login_ban_timein_exists(Id) of
        true ->
            ?WARN(?_U("玩家帐号:~p已经被禁!"), [Id]),
            throw({stop, normal});
        false ->
            ok
    end,

    State1 =
    case gm_ctrl:is_tracked(Id) of
        true ->
            ?WARN(?_U("玩家帐号:~p开始被跟踪!"), [Id]),
            State#client_state{track = true};
        false ->
            State
    end,
    % 心跳检测
    start_heart_timer(),

    % 注册名称
    case player_server:enter(Id, self(), Ip, OpenKey, Platform, Other) of
        {ok, Player} ->
            ?WARN(?_U("玩家:~p pid:~p 进入游戏"), [Id, Player]),
            true = erlang:register(?NAME(Id), self()),
            ?IF(FirstEnter,
                ok = db_account:set_status(AccName, Sid, ?ACCOUNT_STATUS_ENTER),
                ok),
            ok = send_enter_s2c(MsgId, ?E_OK, StateName, State1),
            mod_fcm:notify_change_cm(Id, Cm),
            mod_player:notify_send_main_role_info(Id),
            % 添加到client mgr
            ok = gate_client_mgr:add(Id, self()),
            {ok, ?PLAYING, State1#client_state{id = Id, player_pid = Player}};
        {error, Code} ->
            ?ERROR(?_U("玩家:~p进入游戏出错:~p"), [Id, Code]),
            % 向client发送错误消息
            ok = send_enter_s2c(MsgId, Code, StateName, State1),
            {ok, ?WAIT_PLAY, State1}
    end;

%% 时间同步请求
do_acc_process(_MF, MsgId, #acc_servertime_c2s{}, StateName, State = #client_state{}) ->
    ok = send_msg_server_time(MsgId, StateName, State),
    {ok, ?WAIT_PLAY, State};
do_acc_process(_MF, MsgId, Req, StateName, State) ->
    case catch gate_acc:handle(Req, State) of
        ok ->
            {ok, ?WAIT_PLAY, State};
        {ok, Msg} ->
            ok  = do_send_msg({MsgId, Msg}, StateName, State),
            {ok, ?WAIT_PLAY, State};      
        {'EXIT', _Reason} ->
            ?ERROR(?_U("handle acc request error!:~p"), [_Reason]),
            {stop, normal, ?WAIT_PLAY, State}
    end.

%% PLAYING状态下处理玩家请求
%% 时间同步请求
do_process(_MF, MsgId, #acc_servertime_c2s{}, StateName, State) ->
    ok = send_msg_server_time(MsgId, StateName, State),
    State2 = anti_fps_speedup(StateName, State),
    {ok, State2};
%% 其它请求
do_process({_Mod, _}, _MsgId, Req, _StateName,
    #client_state{id = _Id, switch_node = true} = State) ->
    ?WARN(?_U("切换节点时,玩家:~p收到其它请求:~p"), [_Id, Req]),
    {ok, State};
do_process({Mod, _}, MsgId, Req, _StateName,
    #client_state{id = Id, track = Track, switch_node = false, player_pid = PlayerPid} = State) ->
    ?IF(Track, peer_log:c2s_msg(Id, MsgId, Req), ok),
    ok = player_server:process(Mod, Req, MsgId, PlayerPid),
    {ok, State}.

%%----------------
%% 关于socket处理
%%----------------

%% socket关闭
when_sock_closed(_Sock, ?WAIT_LOGIN, _State) ->
    exit(normal);
when_sock_closed(_Sock, ?WAIT_PLAY, State) ->
    #client_state{
        accname = Accname,
        ip = Ip,
        connect_time = CTime
    } = State,
    ok = game_log:gateway_closed_before_play(Accname, Ip, wg_util:now_sec() - CTime),
    exit(normal);
when_sock_closed(_Sock, _StateName, State) ->
    ok = game_log:gateway_closed(State#client_state.accname, State#client_state.ip),
    exit(normal).

%% socket发生错误
when_sock_error(_Sock, _Reason, ?WAIT_LOGIN, #client_state{ip = IP}) ->
    ?ERROR(?_U("连接 ip:~p 出错:~p"), [IP, _Reason]),
    exit(normal);
when_sock_error(_Sock, _Reason, _StateName, #client_state{accname = AccName} = State) ->
    ?ERROR(?_U("玩家(accname):~p socket:~p 出错:~p"), [AccName, _Sock, _Reason]),
    ok = game_log:gateway_error(AccName, State#client_state.ip, _Reason),
    exit(normal).

%% 向client发送数据
%% Msg格式为record
do_send_msg({MsgId, Record}, StateName, #client_state{sock = Sock, id = _Id, log = _Log, track = _Track} = State) ->
    ?IF(_Log =:= 0,
        ?IF(_Track, peer_log:s2c_msg(_Id, MsgId, Record), ok),
        ?IF2(element(1, Record) =/= acc_servertime_s2c, ?DEBUG("gate ~p send msg ~p~n~p", [_Id, MsgId, Record]))),
    {Packet, _Size} = proto:en_packet(MsgId, Record),
    %?DEBUG(?_U("向玩家:~p 发送数据:~p"), [?IF(_Id =:= undefined, "unknown", _Id), Packet]),
    % 加密
    %Cliper = gate_crypt:encrypt(Packet),
    % 发送数据
    do_send_data(Sock, Packet, StateName, State).

%% 发送数据,减少selective receive
do_send_data(Sock, Data, StateName, State = #client_state{health = 0}) ->
    try 
        case erlang:port_command(Sock, Data, [force]) of
            true ->
                ok;
            false ->
                % busy
                do_check_tcp_close_or_error(Sock, StateName, State),
                ?WARN(?_U("玩家:~p端口繁忙(~p)"), 
                    [State#client_state.id, State#client_state.ip]),
                ok
        end
    catch
        _:_Error ->
            ?WARN(?_U("玩家:~p(~p)数据发送错误 ~p"),
                [State#client_state.id, State#client_state.ip, _Error]),
            when_sock_error(Sock, einval, StateName, State)
    end;

do_send_data(Sock, Data, StateName, State0) ->
    State = State0#client_state{health = 0},
    do_send_data(Sock, Data, StateName, State).

%% 重新设置active
do_active(Sock) ->
    ok = inet:setopts(Sock, [{active, once}]).

%%-----------------
%% 合并发送网络数据
%%-----------------

%% 合并发送给玩家的数据(优化)
%% 如果距离上次发送小于一定时间则缓存
%% port_command消耗比较大(80us),网络发送次数,有好处.
do_send_msg_merge({MsgId, Msg}, StateName, #client_state{id = _Id, log = _Log, track = _Track} = State) ->
    ?IF(_Log =:= 0,
        ?IF(_Track, peer_log:s2c_msg(_Id, MsgId, Msg), ok),
        ?IF2(element(1, Msg) =/= acc_servertime_s2c, ?DEBUG("gate ~p send msg ~p~n~p", [_Id, MsgId, Msg]))),
    {Packet, Size} = proto:en_packet(MsgId, Msg),
    %Cipher = gate_crypt:encrypt(Packet),
    %?DEBUG(?_U("消息被缓存...:~p"), [Msg]),
    add_prepare_send_msg(Packet, Size, StateName, State).

%% 启动flush缓存消息timer
start_flush_prepare_msg_timer() ->
    erlang:send_after(?PACKET_MERGE_TIME, self(), flush_prepare_msg).

%% 发送缓存的消息
do_flush_prepare_msg(StateName, State) ->
    case take_prepare_send_msg() of
        [] ->
            ok;
        Data ->
            %?DEBUG(?_U("被发送的数据(~p)...:~p"), [iolist_size(Data), Data]),
            do_send_data(State#client_state.sock, Data, StateName, State)
    end.

%% 累积待发送数据
-define(PREPARE_SEND_MSG, '!prepare_send_msg').
%% 累积待发送数据长度
-define(PREPARE_SEND_MSG_SIZE, '!prepare_send_msg_size').

%% 获取待发送消息长度 
get_prepare_send_msg_size() ->
    case erlang:get(?PREPARE_SEND_MSG_SIZE) of
        undefined ->
            0;
        Size ->
            Size
    end.

%% 设置待发送消息长度
set_prepare_send_msg_size(Size) ->
    erlang:put(?PREPARE_SEND_MSG_SIZE, Size),
    ok.

%% 添加累积发送数据
add_prepare_send_msg(Msg, MsgSize, StateName, State) ->
    PrevSize = get_prepare_send_msg_size(),
    if
        MsgSize + PrevSize > ?PACKET_SIZE_MAX ->
            % 需要立刻发送数据
            do_flush_prepare_msg(StateName, State),
            list_add(?PREPARE_SEND_MSG, Msg),
            set_prepare_send_msg_size(MsgSize);
        true ->
            list_add(?PREPARE_SEND_MSG, Msg),
            set_prepare_send_msg_size(PrevSize + MsgSize)
    end.

%% 获取并清理累积发送数据
take_prepare_send_msg() ->
    case erase_list(?PREPARE_SEND_MSG) of
        undefined ->
            [];
        Data ->
            lists:reverse(Data)
    end.


%%-------------
%% 关于超时心跳
%%-------------

%% 启动heart定时器
start_heart_timer() ->
    %?DEBUG("start heart check timer"),
    erlang:start_timer(?HEART_TIME, self(), heart_check).

%% 进行超时检测
do_heart_check(StateName, State = #client_state{sock = Sock}) ->
    #client_state{
        heart_n = HeartN, 
        heart_t = HeartT
    } = State,
    Ret = inet:getstat(Sock, [recv_cnt]),
    ?IF(Ret =:= {error, einval},
        when_sock_error(Sock, einval, StateName, State),
        ok),
    {ok, [{recv_cnt, RecvCnt}]} = Ret,
    State2 = 
    case HeartN =/= RecvCnt of
        true -> 
            State#client_state{heart_n = RecvCnt, heart_t = 0};
        false -> % not receive new packets
            HeartT2 = HeartT + 1,
            State#client_state{heart_t = HeartT2}
    end,
    _ = start_heart_timer(),
    case State2#client_state.heart_t =:= ?HEART_TIMEOUT_MAX of
        true -> % 达到最多超时次数
            ?INFO(?_U("心跳检测,玩家(平台id:~p)结束"), [State2#client_state.accname]),
            ok = game_log:gateway_timeout(State2#client_state.accname, State2#client_state.ip),
            {stop, normal, State2};
        false ->
            {next_state, StateName, State2}
    end.

%%--------------------
%% 关于client发包限制
%%--------------------

%% 确保发包字节和数目不超过限制
-define(PACKET_PER_SEC_MAX, 15).   % 每秒钟收包最大数
-define(BYTE_PER_SEC_MAX, 2000).   % 每秒钟收字节最大数

%% 启动流量抽检timer
%% 5-60秒随机抽检
start_traffic_timer() ->
    Time = game_misc:rand(30, 35),
    erlang:start_timer(Time * 1000, self(), traffic_check).

%% 设置上一次流量检测数据
set_prev_traffic_data(Packet, Byte, Time) ->
    put(traffic_data, {Packet, Byte, Time}),
    ok.

%% 获取上一次流量检测数据
get_prev_traffic_data() ->
    case get(traffic_data) of
        undefined ->
            ?NONE;
        Data ->
            Data
    end.

%% 设置连续流量超标次数
set_traffic_over_count(N) ->
    erlang:put(traffic_over_count, N),
    ok.

%% 获取连续流量超标次数
get_traffic_over_count() ->
    case erlang:get(traffic_over_count) of
        undefined ->
            0;
        N ->
            N
    end.

%% 进行流量检测
%% 1,获取当前流量数据
%% 2,是否为第一次流量统计?
%%  2a,是,啥都不做
%%  2b,计算流量(每秒包数和字节),判断是否超过限制
%% 3,启动检测timer
do_traffic_check(#client_state{sock = Sock}) ->
    % 1
    case inet:getstat(Sock, [recv_cnt, recv_oct]) of
        {error, Error} ->
            ?WARN(?_U("进行流量检测时,获取socket信息出错:~p"), [Error]),
            RecvPacket = 0, % 无用,只为去除编译警告
            RecvByte = 0,   % 无用,只为去除编译警告
            throw(ok);
        {ok, [{recv_cnt, RecvPacket}, {recv_oct, RecvByte}]} ->
            ok
    end,
        
    % 2
    case get_prev_traffic_data() of
        ?NONE ->
            % 2a
            ok;
        {PrevPacket, PrevByte, PrevTime} ->
            % 2b
            Packet = RecvPacket - PrevPacket,
            Byte = RecvByte - PrevByte,
            Time = wg_util:now_sec() - PrevTime,
            PacketPS = round(Packet / Time),
            BytePS = round(Byte / Time),
            %?INFO(?_U("玩家每秒发包流量:~p包/s ~p字节/s"),
            %    [PacketPS, BytePS]),
            Count = get_traffic_over_count(),
            if
                PacketPS > ?PACKET_PER_SEC_MAX ->
                    Count2 = Count + 1,
                    Code = ?E_TRAFFIC_PACKET_LIMIT;
                BytePS > ?BYTE_PER_SEC_MAX ->
                    Count2 = Count + 1,
                    Code = ?E_TRAFFIC_BYTE_LIMIT;
                true ->
                    Count2 = Count,
                    Code = ?E_OK
            end,
            case Count2 =:= ?TRAFFIC_OVER_MAX of
                true ->
                    ?C2SERR(Code);
                false ->
                    set_traffic_over_count(0)
            end
    end,
    % 3
    set_prev_traffic_data(RecvPacket, RecvByte, wg_util:now_sec()),
    start_traffic_timer(),
    ok.

%%----------
%% 消息相关
%%----------

%% 登录应答
send_login_s2c(MsgId, Code, StateName, State) ->
    Msg = #acc_login_s2c{code = Code},
    do_send_msg({MsgId, Msg}, StateName, State).

%% 游客应答
send_guest_s2c(MsgId, Code, StateName, State) ->
    Msg = #acc_guest_login_s2c{code = Code},
    do_send_msg({MsgId, Msg}, StateName, State).

%% 游客登录发送角色
send_guest_role_s2c(Id, Career, Sex, StateName, State) ->
    Msg = #acc_role_s2c{state = ?ROLE_STATE_NEWBIE, 
        id = Id, 
        career = Career, 
        sex = Sex
    },
    MsgId = ?PROTO_CONVERT({mod_acc, role}),
    do_send_msg({MsgId, Msg}, StateName, State).

%% 发送进入游戏的应答
send_enter_s2c(MsgId, Code, StateName, State) ->
    Msg = #acc_enter_s2c{code = Code},
    do_send_msg({MsgId, Msg}, StateName, State).

-define(LAST_SERVER_TIME, 'last_server_time').
%% 发送服务器时间
send_msg_server_time(MsgId, StateName, State) ->
    Time = wg_util:now_ms(),
    Msg = #acc_servertime_s2c{time = Time},
    do_send_msg({MsgId, Msg}, StateName, State).

%% 检测是否频率加速
anti_fps_speedup(StateName, State) ->
    #client_state{anti_fps = AntiTimes, id = _Id, ip = _IP} = State,
    % 如果连续三次检测到帧频不正常，就向客户端发送anti协议
    Time = wg_util:now_ms(),
    Tick = get_heartbeat_tick(Time),
    %?DEBUG(?_U("请求服务器时间间隔:~p"), [Tick]),
    if
        Tick < 2500 ->
            AntiTimes2 = AntiTimes + 1;
        true ->
            AntiTimes2 = 0
    end,
    if
        AntiTimes2 >= 8 -> 
            MsgId = ?PROTO_CONVERT({mod_acc, anti_spam}),
            Msg = #acc_anti_spam_s2c{type = 1}, 
            %?INFO(?_U("检测到玩家:~p ip:~p 疑似在使用频率加速作弊，请关注"),
            %    [Id, IP]),
            ok = do_send_msg({MsgId, Msg}, StateName, State);
        true ->
            ok
    end,
    State#client_state{anti_fps = AntiTimes2}.

%% 获取本次心跳间隔
get_heartbeat_tick(CurTime) ->
    case erlang:get(?LAST_SERVER_TIME) of
        undefined ->
            erlang:put(?LAST_SERVER_TIME, CurTime),
            CurTime;
        T ->
            erlang:put(?LAST_SERVER_TIME, CurTime),
            CurTime - T
    end.

%% 检测socket是否关闭或出错
do_check_tcp_close_or_error(Sock, StateName, State) ->
    receive 
        {tcp_closed, Sock} ->
            when_sock_closed(Sock, StateName, State);
        {tcp_error, Sock, _Reason} ->
            when_sock_error(Sock, _Reason, StateName, State)
    after 
        0 ->
            ok
    end.
