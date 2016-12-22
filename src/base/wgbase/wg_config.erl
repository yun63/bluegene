%%%----------------------------------------------------------------------
%%%
%%% @copyright wg 2009
%%%
%%% @author litaocheng@gmail.com
%%% @doc handle the configure info(use ets), the config file format is
%%%  the erlang term:
%%%  {key1, value1}.
%%%  {key2, value2}.
%%%  
%%%  {include, "dir/*.conf"}.
%%%  可以用来包含子conf
%%%
%%%----------------------------------------------------------------------
-module(wg_config).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("wg_internal.hrl").
-include_lib("kernel/include/file.hrl").
-behaviour(gen_server).

-export([start_link/2, start_link/3]).
-export([start/3, stop/1]).
-export([i/1, get/2, get/3, set/3, reload/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).

-record(state, {
        name,           % 名称
        file,           % 对应文件
        reload,         % 重新加载检测时间
        last            % 上次加载时间
    }).
        
%% @doc start the config
%% Reload单位为秒,如果为0则不进行config检测
%% 依据File的时间戳,来决定是否重新加载
start_link(Name, File) ->
    start_link(Name, File, 0).
start_link(Name, File, Reload) when is_integer(Reload), Reload >= 0 ->
    ?DEBUG(?_U("启动config服务:~p对应文件:~ts自动检测时间:~p"), 
        [Name, File, Reload]),
    gen_server:start_link({local, Name}, ?MODULE, {Name, File, Reload}, []).

%% @doc 启动config server
start(Name, File, Reload) ->
    ?DEBUG(?_U("启动config服务:~p对应文件:~ts自动检测时间:~p"), 
        [Name, File, Reload]),
    gen_server:start({local, Name}, ?MODULE, {Name, File, Reload}, []).

%% @doc 停止config server
stop(Name) ->
    gen_server:call(Name, stop).

%% @doc 运行信息
i(Name) ->
    call(Name, i).

%% @doc 获取某个配置
get(Name, Key) ->
    case ets:lookup(Name, Key) of
        [{Key, Val}] -> 
            Val;
        [] ->
            undefined
    end.

%% @doc get the key, if the key not exist return the default value
get(Name, Key, Def) ->
    case get(Name, Key) of
        undefined ->
            Def;
        Val ->
            Val
    end.

%% @doc 设置某个key对应的value
set(Name, Key, Val) ->
    call(Name, {set, Key, Val}).

%% @doc 重新加载配置文件
reload(Name) ->
    call(Name, reload).

%%
%% gen_server callbacks
%%
init({Name, File, Reload}) ->
    Name = ets:new(Name, [set, protected, named_table,
            {keypos, 1},
            ?ETS_CONCURRENCY
            ]),
    State = #state{name = Name, file = File, reload = Reload * 1000},
    case do_load(State) of
        ok ->
            ?IF(Reload =/= 0, start_reload_timer(State), ok),
            {ok, State#state{last = stamp()}};
        Ret ->
            {stop, Ret}
    end.

handle_call(stop, _From, State) ->
    {reply, ok, normal, State};
handle_call(i, _From, State) ->
    Info = do_i(State),
    {reply, Info, State};
handle_call({set, Key, Val}, _From, #state{name = Name} = State) ->
    true = ets:insert(Name, [{Key, Val}]),
    {reply, ok, State};
handle_call(reload, _From, State) ->
    case do_load(State) of
        ok ->
            {reply, ok, State#state{last = stamp()}};
        Reply ->
            {reply, Reply, State}
    end;
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(do_auto_reload, State) ->
    ok = do_auto_reload(State),
    start_reload_timer(State),
    {noreply, State#state{last = stamp()}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
    
%%-----------------------------------------------------------------------------
%% internal API
%%-----------------------------------------------------------------------------

%% 调用gen_server
call(Name, Req) ->
    gen_server:call(Name, Req).

%% 内部信息
do_i(#state{file = File, last = Time}) ->
    [{file, File},
    {time, Time}].

%% 加载配置并插入到ets中
do_load(#state{name = Name, file = FileName}) ->
    case wg_util:consult_nested(FileName) of
        {ok, L} ->
            [true = ets:insert(Name, KV) || KV <- L],
            ok;
        Ret ->
            Ret
    end.

%%--------------
%% 关于reload
%%--------------

start_reload_timer(#state{reload = Reload}) ->
    erlang:send_after(Reload, self(), do_auto_reload).

%% 时间戳
stamp() ->
    erlang:localtime().

%% 尝试自动加载文件
do_auto_reload(#state{last = Last, file = Filename} = State) ->
    Now = stamp(),
    case file:read_file_info(Filename) of
         {ok, #file_info{mtime = Mtime}} when Mtime >= Last, Mtime < Now ->
             do_load(State);
         {ok, _} ->
             ok;
         {error, Reason} ->
             ?ERROR("Error reading ~s's file info: ~p", [Filename, Reason])
    end,
    ok.
    
%%-------------
%% EUnit TEST
%%-------------

-ifdef(EUNIT).
-endif.
