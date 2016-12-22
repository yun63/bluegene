%%%----------------------------------------------------------------------
%%%
%%% @date 2011-01-30
%%% @author yys159258@126.com
%%% @doc 是否显示箱子
%%% @end
%%%----------------------------------------------------------------------
-module('_http_sys_box').
-author('YuSongYang').
-vsn('0.1').
-include("wg.hrl").
-include("wg_httpd.hrl").
-include("wg_log.hrl").

-export([handle/2]).

handle(Req,Method)->
    Qs =
        case Method of
            'GET' ->
            Req:parse_qs();
            'POST' ->
            Req:parse_post()
        end,
    Ctrl = ?QS_GET("ctrl", Qs, "1"),
    ok = player_server:admin_ctrl_box(?S2N(Ctrl)),
    {200, [], "ok"}.
