-module(world_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% @doc start world application
start(_StartType, _StartArgs) ->
    try
        %ebase_loglevel:set(?LOG_LEVEL),
        ok = ensure_apps(),
        start_global_managers(_StartType, _StartArgs)
    catch
        Type:Error ->
            io:format("启动world_app失败:~p: ~p~n", [Type, Error]),
            init:stop(world_app)
    end.


%% @doc 停止world应用程序,保存管理器数据
stop(_State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @doc world_app依赖其他应用程序,所以在开启world_app之前必须确保其所依赖的应用程序全部成功启动
ensure_apps() ->
    ok.


%% @doc 启动全局管理器
start_global_managers(_Type, _Args) ->
    %?INFO("启动world_sup"), []),
    {ok, Sup} = world_sup:start_link(),
    % 启动配置管理器
    ok = start_configuration_mgr(Sup),
    % 启动reloader管理器
    ok = start_reloader_mgr(Sup),
    % 启动时间缓冲管理器
    %ok = start_time_cache(Sup),
    % 启动缓存管理器
    %ok = start_cache_mgr(Sup),
    % 启动gm管理器
    %ok = start_gm_ctrl(Sup),
    % 启动排行榜管理器
    %ok = start_rank_mgr(Sup),
    % 启动聊天管理器
    %ok = start_chat_mgr(Sup),

    {ok, Sup}.

start_configuration_mgr(Sup) ->
    Child = {configuration_mgr, {configuration_mgr, start_link, []},
             permanent, brutal_kill, worker, [configuration_mgr]},
    game_util:start_child(Sup, Child).

start_reloader_mgr(Sup) ->
    Child = {reloader_mgr, {reloader_mgr, start_link, []},
             permanent, brutal_kill, worker, [reloader_mgr]},
    game_util:start_child(Sup, Child).





