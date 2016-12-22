%%%----------------------------------------------------------------------
%%%
%%% @copyright wg
%%%
%%% @author songze.me@gmail.com
%%% @doc the extended mofule include some lists processing
%%% @end
%%%----------------------------------------------------------------------
-module(wg_lists).
-author('songze.me@gmail.com').
-vsn('0.1').
-include("wg_internal.hrl").

-export([rand_pick/1, rand_pick/2, rand_pick/3, delete/2, shuffle/1]).
-export([pos/2, keypos/3, replace/3]).
-export([keyupdate/4]).
-export([subtract/2]).
-export([get_by_page/3,get_total_page/2]).
-export([nth/2, index_of/2, join/2]).
-export([fold_by_n/3, run_times/2]).

%% @doc random pick one element from the list.
%%     the List must not be empty
-spec rand_pick(List :: [any(), ...]) -> any().
rand_pick([_|_] = List) ->
    Pos = random:uniform(length(List)),
    lists:nth(Pos, List).

%% @doc 从列表中随机选取N个元素,如果列表中元素少于等于N,
%% 则返回所有元素
rand_pick(N, List) ->
    rand_pick(N, length(List), List).
rand_pick(0, _Len, _List) ->
    [];
rand_pick(N, Len, List) ->
    if
        Len =< N ->
            List;
        true ->
            rand_pick1(N, List, Len, [])
    end.

rand_pick1(0, _List, _Len, Result) ->
    Result;
rand_pick1(_, [], _Len, Result) ->
    Result;
rand_pick1(N, List, Len, Acc) ->
    Pos = random:uniform(Len),
    Elem = lists:nth(Pos, List),
    List2 = lists:delete(Elem, List),
    rand_pick1(N-1, List2, Len-1, [Elem|Acc]).

%% @doc 删除列表中第N个元素,如果列表长度小于N,什么都不做
delete(N, List) ->
    delete3(N, List, 1).
delete3(N, [_|T], N) ->
    T;
delete3(N, [H|T], Pos) ->
    [H | delete3(N, T, Pos+1)];
delete3(_N, [], _) ->
    [].

%% @doc 将一个列表随机打乱
shuffle([]) -> [];
shuffle(L) ->
    lists:sort(
    fun(_, _) ->
        random:uniform(2) rem 2 =:= 0
    end, L).

%% @doc return the position of the item whose Nth element
%%   compares equal to Key. return 0 if not found
-spec keypos(Key :: term(), N :: pos_integer(), L :: list()) ->
    non_neg_integer().
keypos(Key, N, L) ->
    keypos0(Key, N, L, 1). 

keypos0(_Key, _N, [], _Pos) ->
    0;  
keypos0(Key, N, [H|Rest], Pos) ->
    if element(N, H) =:= Key ->
        Pos;
       true ->
        keypos0(Key, N, Rest, Pos+1)
    end.

%% @doc return the pos, when the element in list match the key, return > 0 if found,
%%  otherwise return 0
-spec pos(Key :: any(), L :: list()) -> non_neg_integer().
pos(Key, L) ->
    pos(Key, L, 1).

pos(Key, [Key | _R], Acc) -> Acc;
pos(Key, [_ | R], Acc) -> 
    pos(Key, R, Acc + 1);
pos(_Key, [], _Acc) ->
    0.

%% @doc 对指定元素进行更新
keyupdate(Key, N, List, Fun) when is_integer(N), N > 0, 
        is_function(Fun, 1) ->
        catch keyupdate3(Key, N, List, Fun).

keyupdate3(Key, Pos, [Tup|Tail], Fun) when element(Pos, Tup) =:= Key ->
    [Fun(Tup) | Tail];
keyupdate3(Key, Pos, [H|Tail], Fun) ->
    [H | keyupdate3(Key, Pos, Tail, Fun)];
keyupdate3(_Key, _Pos, [], _Fun) -> % not found
    throw(false). 

%% @doc 替换第N个元素
%% N必须小于length(List)
replace(N, List, NewVal) when N > 0 ->
    {Left, [_|T]} = lists:split(N-1, List),
    Left ++ [NewVal | T].

-spec subtract(list(), list() | any()) ->
    list().
subtract(List1, Elem) when not is_list(Elem) ->
    List1 -- [Elem];
subtract(List1, List2) when is_list(List2) ->
    Set = gb_sets:from_list(List2),
    [E || E <- List1, not gb_sets:is_element(E, Set)].

%% @doc 分页获取列表数据
get_by_page(Page, PerPage, List) when Page >= 1, is_integer(PerPage) ->
    Start = (Page - 1) * PerPage + 1,
    try
        lists:sublist(List, Start, PerPage)
    catch
        _:_ ->
            []
    end.

%% @doc 获取总页数
get_total_page(List, PerPage) ->
	Len = length(List),
    (Len + PerPage - 1) div PerPage.

%% @doc 获取列表的某一项
nth(N, List) ->
    try
        Item = lists:nth(N, List),
        {ok, Item}
    catch
        _:_ ->
            false
    end.

%% @doc 列表中第几个
index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

%% @doc 将list用指定的分隔符组合在一起,返回string list
join([], _Sep) ->
    [];
join([H|T], Sep) ->
    lists:concat([H] ++ lists:append([[Sep, Item] || Item <- T])).

%% @doc fold 基于N
fold_by_n(_Fun, Acc, 0) ->
    Acc;
fold_by_n(Fun, Acc, N) when N > 0 ->
    fold_by_n(Fun, Fun(Acc), N - 1).

%% @doc run times基于N
run_times(_Fun, 0) ->
    ok;
run_times(Fun, N) ->
    Fun(),
    run_times(Fun, N - 1).

%%------------
%% EUnit TEST
%%------------

-ifdef(EUNIT).

keypos_test_() ->
    [
        ?_assertEqual(1, keypos(k1, 1, [{k1, v1}, {k2, v2}])),
        ?_assertEqual(1, keypos(k1, 2, [{dummy, k1, v1}, {dummy, k2, v2}])),
        ?_assertEqual(0, keypos(k1, 1, []))
    ].

pos_test_() ->
    L = [22, k1, k2, make_ref(), k3, k4],
    [
        ?_assertEqual(2, pos(k1, L)), 
        ?_assertEqual(1, pos(22, L)), 
        ?_assertEqual(0, pos(not_exists, L)) 
    ].

%% 随机选择N个元素测试
rand_pic_test_() ->
    [
        ?_assertEqual([1], rand_pick(1, [1])),
        ?_assertEqual([], rand_pick(0, [])),
        ?_assertEqual([], rand_pick(1, [])),
        ?_assertEqual([], rand_pick(0, [1])),

        ?_assertEqual([1, 2], rand_pick(2, [1, 2])),
        ?_assertEqual(3,
            sets:size(sets:from_list(rand_pick(3, [1, 2, 3, 4, 5])))),
        ?_assertEqual(3,
            sets:size(sets:from_list(rand_pick(3, [1, 2, 3, 4, 5, 6, 7, 8])))),
        ?_assertEqual(7,
            sets:size(sets:from_list(rand_pick(7, [1, 2, 3, 4, 5, 6, 7, 8]))))
    ].

%% 删除第N个元素测试
delete_test_() ->
    [
        ?_assertEqual([], delete(1, [])),
        ?_assertEqual([], delete(1, [1])),
        ?_assertEqual([2], delete(1, [1, 2])),
        ?_assertEqual([2,'5'], delete(1, [1, 2, '5'])),
        ?_assertEqual([1,'5'], delete(2, [1, 2, '5'])),
        ?_assertEqual([1, 2,'5'], delete(4, [1, 2, '5']))
    ].

keyupdate_test_() ->
    L = [{foo, 1}, {bar, 2, "2"}, {tee, 3, "3", '3'}],
    FunUpdate = fun(_) -> throw(update_error) end,
    [
        ?_assertMatch([{foo, 2} | _],
            keyupdate(foo, 1, L, fun(_E) -> {foo, 2} end)),
        ?_assertMatch(L, keyupdate(bar, 1, L, fun(E) -> E end)),
        ?_assertEqual(update_error, keyupdate(foo, 1, L, FunUpdate)),
        ?_assertEqual(false, keyupdate(not_exists, 1, L, FunUpdate))
    ].

replace_test_() ->
    [
        ?_assertException(error, _, replace(0, [], 'val')),
        ?_assertEqual(['val'], replace(1, [1], 'val')),
        ?_assertEqual([1, 3, 3], replace(2, [1,2,3], 3)),
        ?_assertEqual([1, 2, 2], replace(3, [1,2,3], 2)),
        ?_assertException(error, _, replace(4, [1,2,3], 'val'))
    ].

subtract_test_() ->
    [
        ?_assertEqual([], subtract([1], 1)),
        ?_assertEqual([1], subtract([1], 2)),
        ?_assertEqual([], subtract([1], [1])),
        ?_assertEqual([1], subtract([1], [2])),

        ?_assertEqual([1, 2], subtract([1, 2, {foo, 1}], [{foo, 1}])),
        ?_assertEqual([1, {foo, 1}], subtract([1, 2, 2, {foo, 1}], [2]))
    ].

%% 分页测试
p(N) ->
    {p, N}.

p_list(N1, N2) ->
    [p(I) || I <- lists:seq(N1, N2)].

page_test_() ->
    PerPage = 6,
    [
        ?_assertEqual([], get_by_page(1, PerPage, [])),
        ?_assertEqual([p(1)], get_by_page(1, PerPage, [p(1)])),
        ?_assertEqual(p_list(1, PerPage),
            get_by_page(1, PerPage, p_list(1, PerPage))),

        ?_assertEqual(p_list(1, PerPage), 
            get_by_page(1, PerPage, p_list(1, PerPage*2+1))),

        ?_assertEqual(p_list(PerPage+1, PerPage*2),
            get_by_page(2, PerPage, p_list(1, PerPage*2+1)))
    ].

%% fold_by_n测试
fold_by_n_test_() ->
    [
        ?_assertEqual(2, fold_by_n(fun(1) -> 1 end, 2, 0)),
        ?_assertEqual(2, fold_by_n(fun(1) -> 2 end, 1, 1)),
        ?_assertEqual(2, fold_by_n(fun(N) -> N * 2 end, 1, 1)),
        ?_assertEqual(16, fold_by_n(fun(N) -> N * 2 end, 1, 4)),
        ?_assertEqual({ok, 3}, fold_by_n(fun({ok, N}) -> {ok, N + 1} end, {ok, 0}, 3))
    ].


-endif.
