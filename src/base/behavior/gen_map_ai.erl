%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2011.07.28
%%% @doc   gen_map_ai behaviour
%%%     定义地图ai behaviour
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(gen_map_ai).
-include("wg.hrl").
-include("const.hrl").
-include("common.hrl").
-include("map.hrl").
-include("area_internal.hrl").

-compile([export_all]).
-export([behaviour_info/1]).
-export([start_timer/2, cancel_timer/1]).

behaviour_info(callbacks) ->
    [
    {on_init, 1},           % 当地图初始化完成
    {handle_timeout, 2},    % 处理timeout
    {handle_call, 2},       % 处理call请求
    {handle_cast, 2},       % 处理cast请求
    {on_set_player, 2},     % 当添加新玩家
    {on_del_player, 2},     % 当删除玩家
    {on_set_mon, 2},        % 当添加怪物
    {on_del_mon, 2},        % 当删除怪物
    {on_change_layer, 2},   % 当地图切换层后
    {on_terminate, 2}       % 当地图准备结束
    ];
behaviour_info(_Other) ->
    undefined.

%% @doc 启动timer
start_timer(Time, Msg) ->
    erlang:send_after(Time, self(), ?MAP_AI_TIMEOUT_EVENT(Msg)).

%% @doc 取消timer
cancel_timer(Ref) when is_reference(Ref) ->
    erlang:cancel_timer(Ref).

%%-----------------------------------------
%% 下面是一个简单的gen_mon_ai behaviour实现
%%-----------------------------------------

%% @doc 当地图初始化完成
-spec on_init(map()) ->
    map().
on_init(Map) ->
    ?DEBUG(?_U("地图:~p初始化完成"), [Map#map.id]),
    start_timer(1000, demo_event),
    Map.

%% @doc 当地图切换层后
-spec on_change_layer(integer(), map()) ->
    map().
on_change_layer(_PrevLayer, Map) ->
    Map.

%% @doc 处理timeout
-spec handle_timeout(any(), map()) ->
    map().
handle_timeout(demo_event, Map) ->
    ?DEBUG(?_U("地图:~p收到demo_event事件"), [Map#map.id]),
    Map.

%% @doc 处理call
-spec handle_call(any(), map()) ->
    {any(), map()}.
handle_call(_Req, Map) ->
    ?DEBUG(?_U("地图:~p收到call请求:~p"), [Map#map.id, _Req]),
    {ok, Map}.

%% @doc 处理cast
-spec handle_cast(any(), map()) ->
    map().
handle_cast(_Req, Map) ->
    ?DEBUG(?_U("地图:~p收到cast请求:~p"), [Map#map.id, _Req]),
    Map.

%% @doc 当添加新的玩家
-spec on_set_player(any(), map()) ->
    any().
on_set_player(_Obj, _Map) ->
    ?DEBUG(?_U("地图:~p中添加新玩家:~p"), [_Map#map.id, _Obj]),
    ok.

%% @doc 当删除玩家
-spec on_del_player(any(), map()) ->
    any().
on_del_player(_Obj, _Map) ->
    ?DEBUG(?_U("地图:~p中删除玩家:~p"), [_Map#map.id, _Obj]),
    ok.

%% @doc 当添加新的怪物
on_set_mon(_Obj, _Map) ->
    ?DEBUG(?_U("地图:~p中添加新怪物:~p"), [_Map#map.id, _Obj]),
    ok.

%% @doc 当删除玩家
on_del_mon(_Obj, _Map) ->
    ?DEBUG(?_U("地图:~p中删除怪物:~p"), [_Map#map.id, _Obj]),
    ok.

%% @doc 当地图要结束时
-spec on_terminate(map(), any()) -> no_return().
on_terminate(Map, _Reason) ->
    ?DEBUG(?_U("地图:~p将要结束,原因:~p"), [Map#map.id, _Reason]),
    ok.
