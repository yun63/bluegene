%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2011.08.01
%%% @doc 数据库操作合并,实现定时写入数据库
%%%     基于进程辞典
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(db_op_merge, [Table]).
-author("litaocheng@gmail.com").
-vsn('0.1').
-include("wg.hrl").
-include("errno.hrl").
-include("const.hrl").
-include("db.hrl").
-include("common.hrl").

-export([list/0, sync/1, sync/3]).
-export([add/2, delete/1, update/2]).
-import(procdict_list, [get_list/1, set_list/2, erase_list/1, list_add/2, list_delete/2,
        list_keydelete/3, list_keyfind/3, list_keyupdate/4]).

%% 合并数据对应的key
-define(DB_MERGE, {'!db_merge_key', Table}).

%% @doc 获取操作列表
list() ->
    L = get_list(?DB_MERGE),
    %?DEBUG(?_U("~p数据库操作列表list:~p"), [Table, L]),
    L.

%% @doc 执行操作
sync(Mod) when is_atom(Mod) ->
    lists:map(
    fun
        ({?DB_OP_ADD, _Key, Record}) ->
            Mod:add(Record);
        ({?DB_OP_DELETE, Key, ?NONE}) ->
            Mod:delete(Key);
        ({?DB_OP_UPDATE, _Key, Record}) ->
            Mod:update(Record)
    end, list()),
    erase_list(?DB_MERGE),
    ok.

sync(FunAdd, FunDelete, FunUpdate) 
    when is_function(FunAdd, 1),
        is_function(FunDelete, 1),
        is_function(FunUpdate, 1) ->
    lists:map(
    fun
        ({?DB_OP_ADD, _Key, Record}) ->
            FunAdd(Record);
        ({?DB_OP_DELETE, Key, ?NONE}) ->
            FunDelete(Key);
        ({?DB_OP_UPDATE, _Key, Record}) ->
            FunUpdate(Record)
    end, list()),
    erase_list(?DB_MERGE),
    ok.

%% @doc 添加记录
%% 获取Key对应记录:
%% * 无 -> 添加,返回ok
%% * add -> 什么都不做,返回false
%% * delete -> 更新原记录,返回ok
%% * update -> 返回false
add(Key, Record) ->
    ?DEBUG(?_U("添加add操作,key:~p record:~p"), [Key, Record]),
    case list_keydelete(?DB_MERGE, Key, 2) of
        false ->
            list_add(?DB_MERGE, {?DB_OP_ADD, Key, Record});
        {?DB_OP_ADD, Key, _RecordOld} ->
            ?ERROR(?_U("表:~p中key:~p对应的记录已经存在"),
                [Table, Key]),
            list_add(?DB_MERGE, {?DB_OP_ADD, Key, _RecordOld}),
            false;
        {?DB_OP_DELETE, Key, ?NONE} ->
            Entry = {?DB_OP_ADD, Key, Record},
            list_add(?DB_MERGE, Entry);
        {?DB_OP_UPDATE, Key, _RecordOld} ->
            ?ERROR(?_U("表:~p中key:~p对应的记录已经存在"),
                [Table, Key]),
            list_add(?DB_MERGE, {?DB_OP_ADD, Key, _RecordOld}),
            false
    end.

%% @doc 删除记录
%% 获取Key对应记录:
%% * 无 -> 返回false
%% * add -> 删除记录,返回ok
%% * delete -> 返回flase
%% * update -> 删除记录,返回ok
delete(Key) ->
    case list_keydelete(?DB_MERGE, Key, 2) of
        false ->
            list_add(?DB_MERGE, {?DB_OP_DELETE, Key, ?NONE});
        {?DB_OP_ADD, Key, _RecordOld} ->
            ok;
        {?DB_OP_DELETE, Key, ?NONE} ->
            false;
        {?DB_OP_UPDATE, Key, _} ->
            ok
    end.

%% @doc 更新记录
%% 获取Key对应记录:
%% * 无 -> 返回false
%% * add -> 转化为添加,返回ok
%% * delete -> 返回flase
%% * update -> 覆盖原有更新,返回ok
update(Key, Record) ->
    ?DEBUG(?_U("添加update操作,key:~p record:~p"), [Key, Record]),
    case list_keydelete(?DB_MERGE, Key, 2) of
        false ->
            list_add(?DB_MERGE, {?DB_OP_UPDATE, Key, Record});
        {?DB_OP_ADD, Key, _RecordOld} ->
            Entry = {?DB_OP_ADD, Key, Record},
            list_add(?DB_MERGE, Entry);
        {?DB_OP_DELETE, Key, ?NONE} ->
            false;
        {?DB_OP_UPDATE, Key, _} ->
            list_add(?DB_MERGE, {?DB_OP_UPDATE, Key, Record})
    end.
