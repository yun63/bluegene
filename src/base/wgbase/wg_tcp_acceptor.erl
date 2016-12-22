%%%----------------------------------------------------------------------------
%%%
%%% @author : litaocheng
%%% @date:  2010.10.22
%%% @doc : tcp acceptor 接受新链接
%%%
%%%----------------------------------------------------------------------------
-module(wg_tcp_acceptor).
-behaviour(gen_server).
-vsn('0.1').
-include("wg_internal.hrl").

%% 由wg_tcp_server调用
-export([start_link/5]).
-export([max_conn/1, current_conn/1, clients/1, refuse_conn/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% tcp accptor 状态
-record(state, {
        lsock, 
        opts,           % client sock选项
        ref,
        callback,       % 当有新连接时，执行的操作
        refuse = false, % 是否禁止接受新连接
        max,            % 最大连接数
        cur = 0         % 当前连接数
    }).

%% @doc 启动acceptor, Opts为client sock设置
start_link(Name, LSock, Opts, Callback, Max) ->
    ?DEBUG("start_link the tcp acceptor:~p~n lsock opts:~p", 
        [Name, inet:getopts(LSock, [active, packet])]),
    gen_server:start_link({local, Name}, ?MODULE, {LSock, Opts, Callback, Max}, []).

%% @doc 获取最大允许连接数
-spec max_conn(pid()) -> pos_integer().
max_conn(Server) ->
    gen_server:call(Server, max_conn).

%% @doc 获取当前连接
-spec current_conn(pid()) -> pos_integer().
current_conn(Server) ->
    gen_server:call(Server, current_conn).

%% @doc 获取client连接列表
clients(Server) ->
    gen_server:call(Server, clients).

%% @doc 拒绝新连接
refuse_conn(Server, Refuse) when is_boolean(Refuse) ->
    gen_server:call(Server, {refuse_conn, Refuse}).

%%
%% gen_server callback
%%
init({LSock, Opts, Callback, Max}) ->
    ?DEBUG("init the acceptor callback:~p max:~p", [Callback, Max]),
    process_flag(trap_exit, true),
    gen_server:cast(self(), accept),
    {ok, #state{lsock = LSock, opts = Opts, callback = Callback, max = Max}}.

handle_call(max_conn, _From, State = #state{max = Max}) ->
    {reply, Max, State};
handle_call(current_conn, _From, State = #state{cur = Cur}) ->
    {reply, Cur, State};
handle_call(clients, _From, State) ->
    {monitors, L} = erlang:process_info(self(), monitors),
    Clients = [Pid || {process, Pid} <- L],
    {reply, Clients, State};
handle_call({refuse_conn, Refuse}, _From, State) ->
    State2 = State#state{refuse = Refuse},
    {reply, ok, State2};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(accept, State) ->
    accept(State);
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({inet_async, _LSock, _Ref, {ok, Sock}}, State = #state{refuse = true}) ->
    ?INFO("refuse new connection!", []),
    gen_tcp:close(Sock),
    accept(State);
handle_info({inet_async, _LSock, _Ref, {ok, Sock}},
            State = #state{cur = Max, max = Max}) -> % reach the max limit
    ?INFO("not accepting new connection, reach max ~p", [Max]),
    gen_tcp:close(Sock),
    accept(State);
handle_info({inet_async, LSock, Ref, {ok, Sock}},
            State = #state{lsock = LSock, opts = Opts, callback = Callback, ref = Ref, cur = Cur}) ->
    % patch up the socket so it looks like one we got from
    % gen_tcp:accept/1
    {ok, Mod} = inet_db:lookup_socket(LSock),
    inet_db:register_socket(Sock, Mod),
    try
        % report
        %{Address, Port} = inet_op(fun () -> inet:sockname(LSock) end),
        %{PeerAddress, PeerPort} = inet_op(fun () -> inet:peername(Sock) end),
        %?INFO("accepted TCP connection on ~s:~p from ~s:~p~n",
        %      [inet_parse:ntoa(Address), Port,
        %       inet_parse:ntoa(PeerAddress), PeerPort]),
        % In the event that somebody floods us with connections we can spew
        % the above message at error_logger faster than it can keep up.
        % So error_logger's mailbox grows unbounded until we eat all the
        % memory available and crash. So here's a meaningless synchronous call
        % to the underlying gen_event mechanism - when it returns the mailbox
        % is drained.
        % gen_event:which_handlers(error_logger),
        % handle
        do_callback(Sock, Opts, Callback),
        accept(State#state{cur = Cur +1})
    catch 
        {inet_error, Reason} ->
            gen_tcp:close(Sock),
            ?ERROR("unable to accept TCP connection: ~p~n",
                   [Reason]),
            accept(State)
    end;

handle_info({inet_async, LSock, Ref, {error, closed}},
            State=#state{lsock=LSock, ref=Ref}) ->
    %% It would be wrong to attempt to restart the acceptor when we
    %% know this will fail.
    ?ERROR("listend socket closed"),
    {stop, normal, State};
handle_info({inet_async, LSock, Ref, {error, Reason}},
            State=#state{lsock = LSock, ref = Ref}) ->
    ?ERROR("receive error on listen socket:~p", [Reason]),
    {stop, normal, State};
handle_info({'DOWN', _MRef, process, _Pid, _Info}, State = #state{cur = Cur}) ->
    %?DEBUG("client process ~p exit reason:~p", [Pid, Info]),
    {noreply, State#state{cur = Cur - 1}};
handle_info(_Info, State) ->
    %?DEBUG("handle unknow info ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%% internal API
%%%

%% 执行操作
do_callback(Sock, Opts, Callback) ->
    ok = inet:setopts(Sock, Opts),
    ?DEBUG("handle the new client:~w opts:~w", [Callback, Opts]),
    Result =
    case Callback of
        {M, F, A} ->
            apply(M, F, [Sock | A]);
        Callback when is_function(Callback, 1) ->
            Callback(Sock)
    end,

    case Result of
        {ok, Pid} ->
            ?DEBUG("started the client process:~p", [Pid]),
            % monitor the client
            _MRef = erlang:monitor(process, Pid),
            % unlink the pid
            true = unlink(Pid),
            case gen_tcp:controlling_process(Sock, Pid) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?ERROR("controlling process error:~p", [Reason]),
                    gen_tcp:close(Sock)
            end;
        {error, _Reason} ->
            %?ERROR("spawn client proces error:~p", [_Reason]),
            catch gen_tcp:close(Sock)
    end.

%inet_op(F) -> wg_util:throw_on_error(inet_error, F).

accept(State = #state{lsock = LSock}) ->
    %% accept more
    case prim_inet:async_accept(LSock, -1) of
        {ok, Ref} -> {noreply, State#state{ref = Ref}};
        Error -> {stop, {cannot_accept, Error}, State}
    end.
