%%%----------------------------------------------------------------------
%%%
%%% @copyright motown 2010
%%%
%%% @author litaocheng@gmail.com
%%% @doc the demo module handle the request path:
%%%  "http://host/demo
%%%----------------------------------------------------------------------
-module('_http_demo').
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("wg.hrl").
-include("wg_httpd.hrl").
-include("wg_log.hrl").

-export([handle/2]).

handle(Req, Method) ->
    ?DEBUG("handle request :~p ~n", [Req]),
    IpStr = Req:get(peer), 
    Headers = Req:get(headers),

    ReqHeaders =
    lists:foldr(fun({K, V}, Acc) ->
        [wg_util:any_to_list(K), " : ", wg_util:any_to_list(V), "\n" | Acc]
    end,
    [],
    mochiweb_headers:to_list(Headers)),
    Qs = Req:parse_qs(),
    Post = Req:parse_post(),

    RspStr = 
    io_lib:format(<<
            "<p>It Works!</p>"
            "<p>Hello, Your IP is ~s method: ~s</p>"
            "<p>post body:~p</p>"
            "<p>Request Data:~p</p>">>,
            [IpStr, Method, Post, Qs]),

    {200, [], [RspStr, ReqHeaders]}.
