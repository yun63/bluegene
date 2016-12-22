%%%----------------------------------------------------------------------
%%%
%%% @author BigBlackBear
%%% @date  2011.3.3
%%% @doc 平台接入接口的验证,未配置的情况下只做参数解析，不做合法性检查
%%% @end
%%%
%%%----------------------------------------------------------------------

-module(game_partner).

-include("wg.hrl").
-include("errno.hrl").
-include("game.hrl").
-include("fcm.hrl").
-include("proto/proto.hrl").
-include("proto/proto_acc.hrl").

-export([auth/1, auth_guest/0, parse_cm/1, identify/3]).

%% @doc 验证登录是否合法
auth(Req) ->
    Mod = get_mod(),
    Mod:auth(Req).

%% @doc 验证游客
auth_guest() ->
    ?CONF(guest_mod, false).

parse_cm(Cm) ->
    Mod = get_mod(),
    Mod:parse_cm(Cm).

%% @doc 防沉迷验证
identify(AccName, Name, Num) ->
    Mod = get_mod(),
    Mod:identify(AccName, Name, Num).

%%-------------------
%% internal API
%%-------------------

%% 获取平台信息
get_mod() ->
    Partner = ?CONF(platform, "dev"),
    list_to_atom(lists:concat(["game_partner_", Partner])).
