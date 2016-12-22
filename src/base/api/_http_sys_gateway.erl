%%%----------------------------------------------------------------------
%%%
%%% @copyright motown 2010
%%% @author litaocheng@gmail.com
%%% @doc 返回网关列表
%%% @end
%%%----------------------------------------------------------------------
-module('_http_sys_gateway').
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("wg.hrl").
-include("wg_httpd.hrl").
-include("wg_log.hrl").

-export([handle/2]).

handle(_Req, _Method) ->
    L = gate_node:get_ip_port(),
    ObjList = 
    [{[{<<"name">>, ?S2B(?A2S(Name))}, {<<"ip">>, ?S2B(IpStr)}, {<<"port">>, Port}]}
    || {Name, IpStr, Port} <- L],
    Data = ejson:encode(ObjList),
    {200, [], Data}.
