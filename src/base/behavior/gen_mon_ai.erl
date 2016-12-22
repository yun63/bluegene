%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2011.06.25
%%% @doc   gen_mon_ai behaviour
%%%     定义怪物ai behaviour
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(gen_mon_ai).
-include("wg.hrl").
-include("const.hrl").
-include("common.hrl").
-include("mon.hrl").
-include("skill.hrl").
-include("area_internal.hrl").

-export([behaviour_info/1]).
-export([all/0, is_exist/1]).
-export([start_timer/2, cancel_timer/1]).
-compile([export_all]).
    
behaviour_info(callbacks) ->
    [
    {on_born, 1},           % 当怪物出生
    {on_damage, 3},         % 当怪物受到伤害
    {on_attack, 4},         % 当怪物攻击后
    {on_back, 1},           % 当怪物准备返回时
    {on_die, 2},            % 当怪物死亡
    {on_move, 1},           % 当怪物移动
    {on_loop, 2},           % 每次怪物循环执行完成
    {on_recover, 1},        % 当怪物回血回蓝时
    {change_type_on_drop, 1},% 当产生掉落时，是否改变怪物的类型(目前，主要作用根据玩家的职业掉对应职业的箱子，召唤魔龙在用)
    {select_box_owner, 1},   % 怪物掉落箱子的所有者
    {on_switch_state, 3},   % 当怪物切换状态
    {can_be_damaged, 1},    % 是否可以被伤害
    {can_enter_fight, 1},   % 是否进入战斗逻辑
    {on_track, 2},          % 是否可以追击
    {select_enemy, 1},      % 选择敌人(攻击目标)
    {select_skill, 2},      % 选择技能
    {is_same_group, 2},     % 是否同一阵营
    {calc_reward, 1},       % 计算奖励
    {buff_lvl, 2},          % 怪物使用AI技能时，给目标加的buff等级
    {handle_call, 2},       % 处理call请求
    {handle_cast, 2},       % 处理cast请求
	{handle_timeout, 2}		% 处理timeout请求
    ];
behaviour_info(_Other) ->
    undefined.

%% @doc 启动timer
start_timer(Time, Msg) ->
	erlang:send_after(Time, self(), ?MON_AI_TIMEOUT_EVENT(Msg)).
	
%% @doc 取消timer
cancel_timer(Ref) when is_reference(Ref) ->
	erlang:cancel_timer(Ref).
	
%% @doc 获取所有的ai模块
all() ->
    Path = filename:join([game_path:root_path(), "ebin"]),
    [begin
        Mod = filename:basename(File, ".beam"),
        ?S2A(Mod)
    end || File <- filelib:wildcard("mon_ai_*.beam", Path)].

%% @doc 判断对应怪物ai模块是否存在
is_exist(AI) when is_atom(AI) ->
    ?DEBUG(?_U("ai:~p all:~p"), [AI, all()]),
    lists:member(AI, all()).

%%-----------------------------------------
%% 下面是一个简单的gen_mon_ai behaviour实现
%%-----------------------------------------

%% @doc 当怪物出生时
-spec on_born(mon()) -> mon().
on_born(Mon) ->
    ?DEBUG(?_U("怪物:~p出生"), [Mon#mon.id]),
    Mon.

%% @doc 当怪物受到伤害
-spec on_damage(mon(), any(), any()) ->
    mon().
on_damage(Mon, _Aer, _DmgInfo) ->
    ?DEBUG(?_U("怪物:~p被:~p伤害:~p"), [Mon#mon.id, _Aer, _DmgInfo]),
    Mon.

%% @doc 当怪物攻击后
-spec on_attack(mon(), any(), any(), boolean()) ->
    mon().
on_attack(Mon, _Der, _DmgInfo, IsDead) ->
    ?DEBUG(?_U("怪物:~p攻击目标:~p伤害结果:~p 目标死亡:~p"), 
        [Mon#mon.id, _Der, _DmgInfo, IsDead]),
    Mon.

%% @doc 当怪物返回时
%% -spec on_back(mon()) -> mon() | {atom(), mon()}.
on_back(Mon) ->
    ?DEBUG(?_U("怪物:~p返回"), [Mon#mon.id]),
    Mon.

%% @doc 当怪物死亡时
-spec on_die(mon(), #aer_info{}) -> mon().
on_die(Mon, _AerInfo) ->
    ?DEBUG(?_U("怪物:~p死亡"), [Mon#mon.id]),
    Mon.

%% @doc 当怪物移动时
-spec on_move(mon()) -> 'ok'.
on_move(_Mon) ->
    ?DEBUG(?_U("怪物:~p移动->(~p,~p)"), 
        [_Mon#mon.id, _Mon#mon.x, _Mon#mon.y]),
    ok.

%% @doc 处理怪物主循环
-spec on_loop(mon(), atom()) -> mon().
on_loop(_Mon, _StateName) ->
    %?DEBUG(?_U("怪物:~p主循环(状态:~p)"), [_Mon#mon.id, _StateName]),
    _Mon.

%% @doc 当怪物切换状态
on_switch_state(_Mon, _From, _To) ->
    %?DEBUG(?_U("怪物:~p从状态:~p切换到:~p"), [_Mon#mon.id, _From, _To]),
    _Mon.

%% @doc 是否可以被伤害
% -spec can_be_damage(mon()) -> boolean().
can_be_damaged(_Mon) ->
    true.

%% @doc 是否进入战斗 
%-spec can_enter_fight(mon()) -> boolean().
can_enter_fight(_Mon) ->
    ?DEBUG(?_U("怪物:~p是否进入战斗流程"), [_Mon#mon.id]),
    true.

%% @doc 怪物选择目标
%% default : 采用默认目标选择方案(从仇恨列表中选择仇恨最大的目标)
%% skip: 跳过此次循环
%% {mon(), aer_info()} : 返回mon,同时指定目标为aer_info
%-spec select_enemy(mon()) ->
%    'default' | 'skip' | {mon(), aer_info()}.
select_enemy(_Mon) ->
    default.

%% @doc 怪物选择技能
%-spec select_skill(mon(), any()) ->
%    'default' | #sys_skill{}.
select_skill(_Mon, _Enemy) ->
    ?DEBUG(?_U("怪物:~p选择技能"), [_Mon#mon.id]),
    default.

%% @doc 怪物死亡奖励
%-spec calc_reward(mon()) -> 
%    'default' | fun().
calc_reward(_Mon) ->
    ?DEBUG(?_U("生成怪物:~p奖励函数"), [_Mon#mon.id]),
    fun
        (Exp, FSoul, SilverBind) ->
            {Exp, FSoul, SilverBind}
    end.

%% @doc 处理call
-spec handle_call(any(), mon()) ->
    {any(), mon()}.
handle_call(_Req, Mon) ->
    ?DEBUG(?_U("怪物:~p收到call请求:~p"), [Mon#mon.id, _Req]),
    {ok, Mon}.

%% @doc 处理cast
-spec handle_cast(any(), mon()) ->
    mon().
handle_cast(_Req, Mon) ->
    ?DEBUG(?_U("怪物:~p收到cast请求:~p"), [Mon#mon.id, _Req]),
    Mon.

%% @doc 处理timeout
-spec handle_timeout(any(), mon()) ->
    mon().
handle_timeout(timeout_auto_die, Mon) ->
	?DEBUG(?_U("怪物:~P存活时间到，主动消亡"), [Mon#mon.id]),
	Mon.
