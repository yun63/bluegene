%%%----------------------------------------------------------------------
%%%
%%% @copyright wg
%%%
%%% @author songze <songze.me@gmail.com>
%%% @doc http client base inet http client
%%%
%%%----------------------------------------------------------------------
-module(wg_httpc).
-author('songze.me@gmail.com').
-vsn('0.1').
-include("wg_internal.hrl").

-export([get/1, get/2, post/2, post/3, https_get/1, https_get/2]).
-define(TIMEOUT, 4000).

%% @doc http get request
-spec get(Url :: string()) ->
    {'ok', any()} | {'error', any()}.
get(Url) ->
    get(Url, ?TIMEOUT).

get(Url, Timeout) ->
    HttpOpts = [{timeout, Timeout}],
    Opts = [{sync, true}, {body_format, binary}, {full_result, false}],
    httpc:request(get, {Url, []}, HttpOpts, Opts).

%% @doc http post request
-spec post(Url :: string(), Data :: iodata()) ->
    {'ok', any()} | {'error', any()}.
post(Url, Data) ->
    post(Url, "application/x-www-form-urlencoded", Data).

%% @doc http post request
-spec post(Url :: string(), Type :: string(), Data :: iodata()) ->
    {'ok', any()} | {'error', any()}.
post(Url, Type, Data) ->
    HttpOpts = [{timeout, ?TIMEOUT}],
    Opts = [{sync, true}, {body_format, binary}, {full_result, false}],
    httpc:request(post, {Url, [], Type, iolist_to_binary(Data)}, HttpOpts, Opts).

https_get(Url) ->
    https_get(Url, ?TIMEOUT).

https_get(Url, Timeout) ->
    HttpOpts = [{timeout, Timeout}],
    Opts = [{sync, true}, {body_format, binary}, {full_result, false}, {ssl, []}],
    httpc:request(get, {Url, []}, HttpOpts, Opts).