%%% -----------------------------------------------------------------------------
%%%
%%% @author yun63 <towardstheway@gmail.com>
%%% @date   2015-04-16
%%%
%%% @doc 基础数据存储模块,由两部分构成:local_data和global_data
%%% local_data : 存储玩家本地数据,以进程字典形式存在于玩家进程中,持久化在mnesia或mysql中
%%% global_data: 全局数据,由独立的进程定时保存,保存在ets中,持久化在mysql中
%%% @end
%%% -----------------------------------------------------------------------------

-module(data_storage).

-include("common.hrl").

-export([declare_data/1, 
         declare_data/2,
         load_player/1,
         save_player/1,
         init/0,
         save/0,
         clear_daily/0,
         clear_weekly/0,
         save_runtime/0,
         recover_runtime/0]).

-define(LOCAL_DATA_DB_DECLARE_MOD, local_data_db_declare_list).
-define(GLOBAL_DATA_DAILY_DECLARE_MOD, global_data_daily_declare_list).

%% @doc 声明数据,服务器启动时调用
declare_data(Sup) ->
    DeclareModList = get_declare_list(),
    % 声明玩家数据
    do_declare_local_data_db(DeclareModList),
    % 声明全局数据
    do_declare_global_data_daily(DeclareModList),
    % 声明其他数据
    do_declare_data(Sup, DeclareModList).



