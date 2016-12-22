%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2010.11.16
%%% @doc 数据库操作接口
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(db, [POOL]).
-author("litaocheng@gmail.com").
-vsn('0.1').
-include("wg.hrl").
-include("db.hrl").
-include("world.hrl").
-include("common.hrl").

-export([fetch/1]).
-export([prepare/2, unprepare/1]).
-export([transaction/1, transaction/2]).
-export([execute/1, execute/2, execute/3]).

-export([select/3, select/4, 
        insert/2, insert/3, insert_multi/2,
        update/4, delete/2, delete/1]).
-export([i/0]).

-import(mysql, [
     get_result_field_info/1,
     get_result_rows/1,
     get_result_affected_rows/1,
     get_result_reason/1]).

%% @doc 执行一条sql语句(可以事务内部也可以外部），
%% 返回{selected, Fields, Rows} 
%% | {error, Reason}
%% | {updated, AffectedRows, InsertId}
fetch(Query) ->
    %?DEBUG(?_U("fetch pool:~p sql:~s"), [POOL, Query]),
    % 记录sql执行情况
    update_sql_statis(Query),
    case catch timer:tc(mysql, fetch, [POOL, Query, ?TIMEOUT]) of
        {Time, Result} when is_integer(Time) ->
            ?IF(Time > 500000, ?WARN("query cost ~p ns ~p", [Time, main_info(Query)]), ok),
            handle_result(Result);
        _Any ->
            handle_result(_Any)
    end.

%% @doc 注册一个名为Name的语句，方便调用
prepare(Name, Query) ->
    mysql:prepare(Name, Query).

%% @doc 取消注册
unprepare(Name) ->
    mysql:unprepare(Name).

%% @doc 执行事务
%% 返回{atomic, Result}
%% | {error, Reason)
%% | {rollback, Reason, Res}.
transaction(Fun) ->
    handle_transaction(catch mysql:transaction(POOL, Fun)).

%% @doc 同上
transaction(Fun, Timeout) ->
    handle_transaction(catch mysql:transaction(POOL, Fun, Timeout)).

%% @doc 执行prepare注册的请求
execute(Name) ->
    handle_result(mysql:execute(POOL, Name)).
execute(Name, Timeout) ->
    handle_result(mysql:execute(POOL, Name, Timeout)).
execute(Name, Params, Timeout) ->
    handle_result(mysql:execute(POOL, Name, Params, Timeout)).

%% @doc 查询请求
%% select * from tab where id = 1;
%% select("tab", "*", "id=1")
select(Table, [H|_] = Fields, Where) 
    when is_list(H), is_list(Where) ->
    Sql = ["select ",
    string:join(Fields, ","),
    " from ", Table, 
    begin
        if 
            length(Where) > 0 ->
                " where " ++ Where;
            true ->
                ""
        end
    end],
    %?DEBUG("sql is: ~s", [iolist_to_binary(Sql)]),
    fetch(Sql).

%% @doc 查询请求
select(Table, [H|_] = Fields, Where, Function)
    when is_list(H), is_list(Where) ->
    Sql = ["select ",
           string:join(Fields, ","),
           " from ", Table,
           begin
               if
                   length(Where) > 0 ->
                        " where " ++ Where;
                   true ->
                        ""
               end
           end,
           begin
               if
                   length(Function) > 0 ->
                        " " ++ Function;
                   true ->
                        ""
               end
           end
          ],
    %?DEBUG("sql is: ~s", [iolist_to_binary(Sql)]),
    fetch(Sql).

%% @doc 插入请求
%% insert into table values(v1, v2)
insert(Table, Vals) ->
    Sql = 
    ["insert into ", Table, 
     " values (", field_join(Vals, ","), ");"],

    % ?DEBUG("sql is: ~s", [iolist_to_binary(Sql)]),
    fetch(Sql).

%% @doc 插入请求
%% insert into table(f1, f2) values(v1, v2)
insert(Table, [H|_] = Fields, Vals) when is_list(H) ->
    Sql = 
    ["insert into ", Table, "(", string:join(Fields, ", "),
     ") values (", field_join(Vals, ","), ");"],

    %?DEBUG("sql is: ~s", [iolist_to_binary(Sql)]),
    fetch(Sql).

%% @doc 插入多条数据
insert_multi(_Table, []) ->
    ok;
insert_multi(Table, ValsList) when is_list(ValsList) ->
    ValsStrList =
    [begin
        "(" ++ field_join(Vals, ",") ++ ")"
    end || Vals <- ValsList],

    Sql = 
    ["insert into ", Table, 
     " values ", string:join(ValsStrList, ","), ";"],

    %?DEBUG("sql is: ~s", [iolist_to_binary(Sql)]),
    fetch(Sql).
    
%% @doc 更新数据
update(Table, [H|_] = Fields, Vals, Where) when is_list(H) ->
    UPairs = lists:zipwith(
    fun(A, B) -> A ++ "=" ++ mysql:encode(B) end,
    Fields, Vals),
    Sql =
    ["update ", Table, " set ",
        string:join(UPairs, ","),
        " where ", Where, ";"],
    %?ERROR("sql is: ~s", [iolist_to_binary(Sql)]),
    fetch(Sql).

%% @doc 删除数据
delete(Table, Where) when is_list(Where) ->
    Sql =
    ["delete from ", Table, 
    begin
        if 
            length(Where) > 0 ->
                " where " ++ Where;
            true ->
                ""
        end
    end],
    %?DEBUG("sql is: ~s", [iolist_to_binary(Sql)]),
    fetch(Sql).
%% @doc 删除所有数据
delete(Table) ->
    Sql =
    ["delete  from ", Table],
    %?DEBUG("sql is: ~s", [iolist_to_binary(Sql)]),
    fetch(Sql).


%%
%% internal API
%%

%% 对结果进行处理
handle_result({data, Result}) ->
    {selected, get_result_field_info(Result), get_result_rows(Result)};
handle_result({updated, Result}) ->
    {updated, get_result_affected_rows(Result)};
handle_result({error, Result}) when is_tuple(Result) ->
    Reason = get_result_reason(Result),
    ?ERROR("mysql error:~s", [Reason]),
    {error, Reason};
handle_result({'EXIT', {Reason, _}}) ->
    ?ERROR("mysql error:~w", [Reason]),
    {error, Reason}.

%% 对事务结果进行处理
handle_transaction(Result) ->
    case Result of
        {atomic, Res} ->
            %?DEBUG("transaction atomic result:~p", [Res]),
            {atomic, Res};
        {aborted, {Err, {rollback_result, Res}}} ->
            %?ERROR("transaction roolback reason:~p result:~p", [Err, Res]),
            {rollback, Err, Res};
        {error, Reason} ->
            %?ERROR("transaction error:~p", [Reason]),
            {error, Reason};
        {'EXIT', Reason} ->
            %?ERROR("transaction error:~p", [Reason]),
            {error, Reason}
    end.

%% 生成字段列表
field_join([], Sep) when is_list(Sep) ->
    []; 
field_join([H|T], Sep) ->
    mysql:encode(H) ++ lists:append([Sep ++ mysql:encode(X) || X <- T]).

%% 根据sql语句,更新统计
update_sql_statis(Sql) ->
    case ?CONF(sql_statis, false) of
        true ->
            SqlStr = lists:flatten(Sql),
            List = string:tokens(SqlStr, " "),
            do_update_sql_statis(List);
        false ->
            ok
    end.

%% 更新统计
do_update_sql_statis(["insert", "into", Table |_]) ->
    world_daily:inc({sql_insert, Table});
do_update_sql_statis(["update", Table |_]) ->
    world_daily:inc({sql_update, Table});
do_update_sql_statis(["delete", "from", Table |_]) ->
    world_daily:inc({sql_delete, Table});
do_update_sql_statis(["delete", "*", "from", Table |_]) ->
    world_daily:inc({sql_delete, Table});
do_update_sql_statis(["select" | Rest]) ->
    Pos = wg_lists:pos("from", Rest),
    Table = lists:nth(Pos + 1, Rest),
    world_daily:inc({sql_select, Table});
do_update_sql_statis(_) ->
    ok.

%% 统计结果
i() ->
    [{K, V} ||
        {{Type, _Table} = K, V} <- world_daily:list(),
        (Type =:= sql_insert orelse Type =:= sql_update orelse Type =:= sql_delete)
    ].

%% 主要信息，打印日志用
main_info([A, B | _]) when is_list(A), is_list(B) ->
    A ++ B;
main_info(_) ->
    "".