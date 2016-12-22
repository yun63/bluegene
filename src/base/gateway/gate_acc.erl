%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2010.11.10
%%% @doc 关于帐号的处理(由gateway调用)
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(gate_acc).
-include("wg.hrl").
-include("game.hrl").
-include("account.hrl").
-include("gate_internal.hrl").
-include("proto/proto_acc.hrl").
-include("proto/proto_debug.hrl").

-export([handle/2, make_random_user/1]).

%% 获取角色列表
handle(#acc_role_c2s{}, #client_state{accname = AccName, sid = Sid}) ->
    Msg =
    try
        {ok, Id} = db_account:get_player_id(AccName, Sid),
        {Career, Sex, MapId} = db_player:get_career_sex_mapid_by_id(Id),
        State =
            ?IF(lists:member(MapId, ?MAP_NEWBIE_LIST),
                ?ROLE_STATE_NEWBIE,
                ?ROLE_STATE_NORMAL),
        #acc_role_s2c{
            state = State,
            id = Id,
            career = Career,
            sex = Sex
        }
    catch
        _ ->
            #acc_role_s2c{
                state = ?ROLE_STATE_NULL,
                id = 0,
                career = 0,
                sex = 0
            }
    end,
    {ok, Msg};

%% 创建角色
handle(#acc_create_c2s{} = Req, #client_state{accname = AccName, sid = Sid, ip = Ip}) ->
    #acc_create_c2s{
        career = Career,
        sex = Sex,
        name = Name
    } = Req,
    ?DEBUG(?_U("创建角色accname:~p 职业:~w 性别:~w 名字:~p"),
        [AccName, Career, Sex, Name]),
    case catch do_create(AccName, Sid, Ip, Name, Career, Sex) of
        {ok, Id} ->
            Code = ?E_OK;
        {error, Code} ->
            Id = 0
    end,
    game_log:create(Id, Name, AccName, Career, Sex),
    Msg = #acc_create_s2c{code = Code, id = Id, career = Career},
    ?DEBUG(?_U("创建角色返回应答:~p"), [Msg]),
    {ok, Msg};

%% 删除角色
handle(#acc_delete_c2s{id = Id}, #client_state{}) ->
    Msg =
    case db_player:delete(Id) of
        ok ->
            #acc_delete_s2c{code = ?E_OK};
        {error, Code} ->
            #acc_delete_s2c{code = Code}
    end,
    {ok, Msg};

%% 新手进度提交，目前此功能无意义
handle(#acc_newbie_submit_c2s{}, #client_state{}) ->
    Msg = #acc_newbie_submit_s2c{code = ?E_OK},
    {ok, Msg};
handle(#debug_ask_log_pre_login_c2s{num1 = Num1, num2 = Num2}, #client_state{accname = AccName}) ->
    game_log:client_ask_log_pre_login(AccName, Num1, Num2),
    ok.

%% @doc 生成一个随机用户
-define(CREATE_GUEST_RETRY, 'create_guest_retry').
make_random_user(#client_state{ip = Ip} = State) ->
    % 目前随机4个角色
    Select = game_misc:rand(4),
    Career = Select,
    Sex = ?SEX_MALE,
    
    % 不做重名检查，名称空间1400万，重了也无所谓
    Name = "游客" ++ random_chars(8),
    % 不做重名检查，名称空间。。。太大了
    AccName = "$guest_" ++ random_chars(16),

    case catch do_create_guest(AccName, Ip, Name, Career, Sex) of
        {error, ?E_NAME_EXIST} ->
            ?WARN(?_U("创建游客错误!")),
            Retry = erlang:get(?CREATE_GUEST_RETRY),
            Retry2 = ?IF(Retry =:= undefined, 0, Retry + 1),
            case Retry2 > 3 of
                true ->
                    ?ERROR(?_U("!!创建游客:~p重试次数过多!"), [AccName]),
                    exit(normal);
                false ->
                    ok
            end,
            erlang:put(?CREATE_GUEST_RETRY, Retry2),
            make_random_user(State);
        {ok, Id} ->
            game_log:guest_create(Id, Name, AccName, Career, Sex),
            {ok, Id, Career, Sex, AccName}
    end.

%% 随机一个字符串序列
random_chars(0) ->
    [];
random_chars(N) ->
    AvailableChars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
    PickOne = lists:nth(game_misc:rand(length(AvailableChars)), AvailableChars),
    [PickOne | random_chars(N-1)].

%%----------------
%% internal API
%%----------------

%% 创建玩家
%% 1,名字合法?否,返回错误
%% 2,职业合法?否,返回错误
%% 3,性别合法?否,返回错误
%% 4,创建玩家
do_create(AccName, Sid, Ip, Name, Career, Sex) ->
    % 1
    ?IF(game_misc:ugc_valid(Name, 2, 6), ok, ?C2SERR(?E_UGC_INVALID)),
    ?IF(words_verify:name_verify(Name), ok, ?C2SERR(?E_UGC_INVALID)),
    
    % 2
    ?IF(lists:member(Career, ?CAREER_LIST), ok,
        ?C2SERR(?E_CAREER_INVAL)),

    % 3
    ?IF(Sex =:= ?SEX_FEMALE orelse Sex =:= ?SEX_MALE, ok,
        ?C2SERR(?E_SEX_INVAL)),

    % 4
    Id = world_id:player(),
    ?DEBUG(?_U("创建玩家:~p对应id:~p"), [AccName, Id]),
    {ok, Id} = db_player:create(Id, AccName, Ip, Name, Career, Sex),
    ok = db_account:set_id_status(AccName, Sid, Id, ?ACCOUNT_STATUS_FINISH_CREATE),
    {ok, Id}.

%% 创建游客玩家
do_create_guest(AccName, Ip, Name, Career, Sex) ->
    {X, Y} = ?CONF(guest_born_point, {?X_INIT, ?Y_INIT}),
    MapId = ?CONF(guest_born_map, ?MAPID_INIT),
    Id = world_id:player(),
    db_player:create(Id, ?S2B(AccName), Ip, ?S2B(Name), Career, Sex, MapId, X, Y).

