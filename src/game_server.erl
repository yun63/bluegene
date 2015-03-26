
-module(game_server).
-export([start/0]).

start() ->
    io:format("~nGame Server Start!~n"),
    ok = application:start(game_server).
