-module(game_util).
-export([start_child/2]).

%% @doc 启动子服务
start_child(Sup, Child) ->
    case catch supervison:start_child(Sup, Child) of
        {ok, _} ->
            ok;
        {error, {{already_started, _Pid}, _}} -> 
            ok;
        Other ->
            io:format("启动子服务: ~p 失败: ~p~n", [Child, Other])
    end.


