%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng@gmail.com
%%% @doc the mnesia db util module
%%%
%%%----------------------------------------------------------------------
-module(wg_mnesia).
-vsn('0.1').

-export([init_db/0]).
-export([dir/0, use_dir/0, is_running/0]).
-export([conn_nodes/1, db_nodes/0, running_nodes/0, extra_nodes/0]).
-export([schema_to_disc/0]).
-export([is_table_exists/1, is_table_empty/1, wild_pattern/1,
        tab_where_to_read/1, tab_where_to_write/1, tab_where_to_commit/1,
        tab_size/1, clear/1,
        dirty_delete_by_pattern/1, dirty_tab2list/1,
        ets_lookup/2, ets_member/2, ets_tab2list/1
    ]).
-export([trans/1, trans/2, dirty/1, dirty/2]).
-export([schema_type/0]).
-export([ensure_mnesia_not_running/0, ensure_mnesia_is_running/0]).
-export([backup_change_nodename/4]).
-export([info/0]).

-include("wg_internal.hrl").

%% @doc init the mnesia db
-spec init_db() -> 'ok'.
init_db() ->
    ensure_mnesia_not_running(),
    case mnesia:create_schema([node()]) of
        {error, {_, {already_exists, _}}} ->
            ok = mnesia:start();
        ok -> %
            ok = mnesia:start()
    end.

%% @doc 获取数据存储路径
dir() ->
    mnesia:system_info(directory).

%% @doc 是否使用了mnesia dir
use_dir() ->
    mnesia:system_info(use_dir).

%% @doc 判断是否运行中,返回yes or no
is_running() ->
    mnesia:system_info(is_running).

%% @doc 连接其他mnesia nodes
conn_nodes(Nodes) ->
    {ok, _} = mnesia:change_config(extra_db_nodes, Nodes),
    ok.

%% @doc 获取db节点
db_nodes() ->
    mnesia:system_info(db_nodes).

%% @doc 获取运行中的db nodes
running_nodes() ->
    mnesia:system_info(running_db_nodes).

%% @doc 获取配置的extra db nodes
extra_nodes() ->
    mnesia:system_info(extra_db_nodes).

%% @doc 修改ram shcema to disc
schema_to_disc() ->
    case schema_type() of
        ram_copies ->
            mnesia:change_table_copy_type(schema, node(), disc_copies);
        disc_copies ->
            ok
    end.

%% @doc 判断表是否存在
is_table_exists(Tab) ->
    lists:member(Tab, mnesia:system_info(tables)).

%% @doc 判断表是否为空
is_table_empty(Tab) ->
    mnesia:dirty_first(Tab) == '$end_of_table'.

%% @doc 获取表格的wild pattern
wild_pattern(Tab) ->
    mnesia:table_info(Tab, wild_pattern).

%% @doc 表格去哪里读取
tab_where_to_read(Tab) ->
    mnesia:table_info(Tab, where_to_read).

%% @doc 表格去哪里写入
tab_where_to_write(Tab) ->
    mnesia:table_info(Tab, where_to_write).

%% @doc 表格去哪里提交
tab_where_to_commit(Tab) ->
    mnesia:table_info(Tab, where_to_commit).

%% @doc 表的size(只能是本地表)
tab_size(Tab) ->
    mnesia:table_info(Tab, size).

%% @doc 清理表中所有数据
clear(Table) ->
    case catch mnesia:clear_table(Table) of
        {atomic, ok} ->
            ok;
        {aborted, _Reason} ->
            ?ERROR(?_U("清理表:~p出错:~p"), [Table, _Reason]),
            {error, _Reason}
    end.

%% @doc 根据模式删除数据
dirty_delete_by_pattern(Pattern) ->
    case catch mnesia:dirty_match_object(Pattern) of
        {'EXIT', {aborted, _Reason}} ->
            ?DEBUG(?_U("模式~w匹配对象出错:~w"), [Pattern, _Reason]),
            {error, _Reason};
        List ->
            [begin
                case catch mnesia:dirty_delete_object(Record) of
                    ok ->
                        ok;
                    {'EXIT', {aborted, _Reason}} ->
                        ?DEBUG(?_U("删除对象~w出错:~w"), [Record, _Reason]),
                        throw({error, _Reason})
                end
            end || Record <- List],
            ok
    end.

%% @doc tab转化list
dirty_tab2list(Table) ->
    Pattern = wild_pattern(Table),
    case catch mnesia:dirty_match_object(Table, Pattern) of
        {'EXIT', {aborted, _Reason}} ->
            ?DEBUG(?_U("模式~w匹配对象出错:~w"), [Pattern, _Reason]),
            {error, _Reason};
        List ->
            List
    end.

%% @doc ets模式从mnesia中查询数据
ets_lookup(Table, Key) ->
    Fun =
    fun(Arg) ->
            ets:lookup(Table, Arg)
    end,
    mnesia:ets(Fun, [Key]).

%% @doc ets模式从mnesia查询数据
ets_member(Table, Key) ->
    Fun =
    fun(Arg) ->
            ets:member(Table, Arg)
    end,
    mnesia:ets(Fun, [Key]).

%% @doc ets模式获取所有数据
ets_tab2list(Table) ->
    Fun =
    fun() ->
            ets:tab2list(Table)
    end,
    mnesia:ets(Fun).

%% @doc transaction执行
trans(Fun) ->
    mnesia:activity(transaction, Fun).
trans(Fun, Args) ->
    mnesia:activity(transaction, Fun, Args).

%% @doc sync_dirty transaction
dirty(Fun) ->
    mnesia:activity(async_dirty, Fun).
dirty(Fun, Args) ->
    mnesia:activity(async_dirty, Fun, Args).

%% @doc get the schema type on the node
-spec schema_type() -> {'error', 'not_exit'} | atom().
schema_type() ->
    case catch mnesia:table_info(schema, storage_type) of
        {'EXIT', {aborted, {no_exists, schema, _}}} ->
            {error, not_exit};
        Type ->
            Type
    end.

%% ensure mnesia not running
-spec ensure_mnesia_not_running() -> 'ok'.
ensure_mnesia_not_running() ->
    case mnesia:system_info(is_running) of
        no ->
            ok;
        _ ->
            mnesia:stop()
    end.

%% ensure mnesia is running
-spec ensure_mnesia_is_running() -> 'ok'.
ensure_mnesia_is_running() ->
    case mnesia:system_info(is_running) of
        yes ->
            ok;
        _ ->
            mnesia:start()
    end.

%% @doc 修改backup数据中的node name
%% From只是旧的节点名称，To为新的节点名字
%% Source指源数据,Target只新数据
backup_change_nodename(From, To, Source, Target) ->
    Switch =
    fun
        (Node) when Node == From ->
            io:format("     - Replacing nodename: '~p' with: '~p'~n", [From, To]),
            To;
        (Node) when Node == To ->
            io:format("     - Node: '~p' will not be modified (it is already '~p')~n", [Node, To]),
            Node;
        (Node) ->
            io:format("     - Node: '~p' will not be modified (it is not '~p')~n", [Node, From]),
            Node
    end,
    Convert =
    fun
        ({schema, db_nodes, Nodes}, Acc) ->
            io:format(" +++ db_nodes ~p~n", [Nodes]),
            {[{schema, db_nodes, lists:map(Switch,Nodes)}], Acc};
        ({schema, version, Version}, Acc) ->
            io:format(" +++ version: ~p~n", [Version]),
            {[{schema, version, Version}], Acc};
        ({schema, cookie, Cookie}, Acc) ->
            io:format(" +++ cookie: ~p~n", [Cookie]),
            {[{schema, cookie, Cookie}], Acc};
        ({schema, Tab, CreateList}, Acc) ->
            io:format("~n * Checking table: '~p'~n", [Tab]),
            Keys = [ram_copies, disc_copies, disc_only_copies],
            OptSwitch =
                fun({Key, Val}) ->
                    case lists:member(Key, Keys) of
                    true ->
                        io:format("   + Checking key: '~p'~n", [Key]),
                        {Key, lists:map(Switch, Val)};
                    false-> {Key, Val}
                    end
                end,
            Res = {[{schema, Tab, lists:map(OptSwitch, CreateList)}], Acc},
            Res;
        (Other, Acc) ->
            {[Other], Acc}
    end,
    mnesia:traverse_backup(Source, Target, Convert, switched).

%% @doc 打印输出mnesia相关信息
info() ->
    ?DEBUG(?_U("*** 节点列表: ~p"), [[node() | nodes()]]),
    ?DEBUG(?_U("*** is_running:~w"), [mnesia:system_info(is_running)]),
    ?DEBUG(?_U("*** extra_nodes:~w"), [extra_nodes()]),
    ?DEBUG(?_U("*** running_nodes:~w"), [running_nodes()]),
    ?DEBUG(?_U("*** local tables: ~p"), [mnesia:system_info(local_tables)]),
    ?DEBUG(?_U("*** tables: ~p"), [mnesia:system_info(tables)]),
    ?DEBUG(?_U("*** system_info:~w"), [mnesia:system_info(all)]),
    ok.
