-module(gateway_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% @doc start gateway application
start(_StartType, _StartArgs) ->
    try 
        %ebase_loglevel:set(?LOG_LEVEL),
        ok = ensure_apps()
    catch
        Type:Error ->
            io:format("启动gateway_app失败:~p: ~p~n", [Type, Error]),
            init:stop(gateway)
    end,

    catch(do_start(_StartType, _StartArgs)).

stop(_State) ->
    io:format("gateway_app服务停止, state: ~p~n", [_State]),
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================

%% @doc ensure some apps must be started
ensure_apps() ->
    %ok = game_util:handle_start_app(db_app:start()),
    %ok = game_util:handle_start_app(log_app:start()),
    ok.

do_start(_StartType, _StartArgs) ->
    {ok, Sup} = gateway_sup:start_link(),
    % 启动其他管理器
    {ok, Sup}.

