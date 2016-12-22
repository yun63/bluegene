%%%----------------------------------------------------------------------
%%%
%%% wg copyright © 2009
%%%
%%% @author songze.me@gmail.com
%%% @doc the odbc module
%%%    config:
%%%     {poolsize, 2}.
%%%     {odbc_keepalive_interval, 60}.
%%%     {odbc_start_interval, 60}. 
%%%     {odbc_server, "DSN=tldh;UID=root;PWD=cheng"}.
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(wg_odbc_sup).
-author('songze.me@gmail.com').
-vsn('0.1').
-include("wg_internal.hrl").
-behaviour(supervisor).

%% API
-export([start_link/1, start_link/2]).
-export([init/1, get_pids/0, get_random_pid/0]).

%% options
-define(ODBC_KEEPALIVE_INTERVAL, 60).
-record(options, {
        poolsize = 2,                   % pool size
        odbc_keepalive_interval = 60,   % seconds
        odbc_start_interval = 60,       % seconds
        odbc_server 
        }).

-define(DEFAULT_POOL_SIZE, 10).
-define(DEFAULT_ODBC_START_INTERVAL, 30). % 30 seconds
-define(SERVER, ?MODULE).

%% @doc start the odbc supervisor
-spec start_link(ODBCServer :: string()) ->
    {'ok', pid()} | 'ignore' | {'error', any()}.
start_link(ODBCServer) ->
    start_link(ODBCServer, []).

-type option() ::
        'poolsize'
        | 'odbc_keepalive_interval'
        | 'odbc_start_interval'
        .
%% @doc start the odbc supervisor
-spec start_link(ODBCServer :: string(), OptsList :: [option()]) ->
    {'ok', pid()} | 'ignore' | {'error', any()}.
start_link(ODBCServer, OptsList) ->
    ?DEBUG("start the wg_odbc_sup DSN:~p opts:~p", [ODBCServer, OptsList]),
    Options  = parse_options(OptsList),
    supervisor:start_link({local, ?SERVER}, ?MODULE, 
            Options#options{odbc_server = ODBCServer}).  

%% do the supervisor init
init(Options) ->
    #options{
        poolsize = PoolSize,
        odbc_keepalive_interval = KeepaliveInterval,
        odbc_start_interval = StartInterval,
        odbc_server = ODBCServer
    } = Options,
    ?INFO("odbc options :~p", [Options]),

    Stragegy = {one_for_one, PoolSize+1, StartInterval}, 
    Mods =
    [
    begin
        {I, {wg_odbc, start_link, 
                    [{KeepaliveInterval, StartInterval*1000, ODBCServer}]},
            transient, brutal_kill, worker, [?MODULE]}
    end || I <- lists:seq(1, PoolSize)],

    {ok, {Stragegy, Mods}}.

get_pids() ->
    [Child || 
        {_Id, Child, _Type, _Modules} <- supervisor:which_children(?SERVER), Child /= undefined].

get_random_pid() ->
    Pids = get_pids(),
    {_, _, N} = now(),
    lists:nth(N rem length(Pids) + 1, Pids).

%% parse options
parse_options(OptsList) ->
    parse_options1(OptsList, #options{}).

parse_options1([{poolsize, N} | Rest], Options) when is_integer(N) ->
    parse_options1(Rest, Options#options{poolsize = N});
parse_options1([{odbc_keepalive_interval, N} | Rest], Options) when is_integer(N) ->
    parse_options1(Rest, Options#options{odbc_keepalive_interval = N});
parse_options1([{odbc_start_interval, N} | Rest], Options) when is_integer(N) ->
    parse_options1(Rest, Options#options{odbc_start_interval = N});
parse_options1([_ | Rest], Options) ->
    parse_options1(Rest, Options);
parse_options1([], Options) ->
    Options.
