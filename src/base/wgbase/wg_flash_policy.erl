%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2010.11.19
%%% @doc 监听843端口，处理flash安全沙箱请求
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(wg_flash_policy).
-author("litaocheng@gmail.com").
-vsn('0.1').
-behaviour(gen_server).
-include("wg.hrl").

-export([start_link/0, start_client/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).

-record(state, {
        sock,           % 对应的sock
        part = <<>>,    % 部分数据
        dummy
    }).

%% FLASH_POLICY_SERVER
-define(FLASH_POLICY_SERVER, 'flash_policy_server').

%% @doc 启动安全请求server
start_link() ->
    IP = "0.0.0.0",
    Port = 843,
    Callback = {?MODULE, start_client, []},
    wg_tcp_server:start_link(?FLASH_POLICY_SERVER, IP, Port, raw, Callback).

%% @doc 处理请求
start_client(Sock) ->
    gen_server:start_link(?MODULE, Sock, []).
    
%%
%% gen_server callbacks
%%
init(Sock) ->
    {ok, #state{sock = Sock}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Sock, ?FLASH_POLICY_REQ}, State = #state{sock = Sock}) ->
    ?DEBUG("received flash policy request", []),
    ok = do_send_policy(Sock),
    {stop, normal, State};
handle_info({tcp, Sock, Data}, State = #state{sock = Sock, part = Part}) ->
    ?ERROR("received parted flash policy request:~p", [Data]),
    Part2 = <<Part/binary, Data/binary>>,
    case Part2 =:= ?FLASH_POLICY_REQ of
        true ->
            ok = do_send_policy(Sock),
            {stop, normal, State};
        false ->
            case byte_size(Part2) > byte_size(?FLASH_POLICY_REQ) of
                true ->
                    {stop, normal, State};
                false ->
                    do_active(Sock),
                    {noreply, State#state{part = Part2}}
            end
    end;
handle_info({tcp_closed, _Sock}, State) ->
    ?INFO(?_U("tcp关闭:~w"), [_Sock]),
    {stop, normal, State};
handle_info({tcp_error, _Sock, _Reason}, State) ->
    ?ERROR(?_U("wg flash tcp socket ~p 出错:~p"), [_Sock, _Reason]),
    {stop, normal, State};
handle_info(_Info, State) ->
    ?ERROR("recv unknown msg:~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ?INFO("process terminated:~p", [_Reason]),
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

%% 发送安全应答
do_send_policy(Sock) ->
    gen_tcp:send(Sock, ?FLASH_POLICY_ACK).

%% 重新设置active
do_active(Sock) ->
    ok = inet:setopts(Sock, [{active, once}]).
