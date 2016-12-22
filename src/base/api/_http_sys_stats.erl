%%%----------------------------------------------------------------------
%%%
%%% @copyright motown 2010
%%% @author litaocheng@gmail.com
%%% @doc the demo module handle the request path:
%%%  "http://host/sys/stats
%%%  返回服务器现在状态
%%% @end
%%%----------------------------------------------------------------------
-module('_http_sys_stats').
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("wg_httpd.hrl").
-include("wg_log.hrl").

-export([handle/2]).

%% @doc 服务器运行状态
handle(Req, _Method) ->
    _Qs = Req:parse_qs(),
    _Post = Req:parse_post(),
    _Nodes = [node() | nodes()],

    List = wg_stats:i(),
    Object = {List},
    Str = ejson:encode(Object),
    {200, [], Str}.
