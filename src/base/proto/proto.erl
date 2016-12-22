%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2010.11.02
%%% @doc 协议处理基本共用模块
%%   所有proto_xxx模块中
%%%  de_xxx 函数参数为binary返回值为{ok, record}
%%%  en_xxx 函数参数为term返回值为iodata
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(proto).
-author("litaocheng@gmail.com").
-vsn('0.1').

-include("wg.hrl").
-include("proto/proto.hrl").

-export([de_packet/1, en_packet/2]).
-export([de_string/1, en_string/1]).
-export([de_loop/3, en_loop/3, en_loop/4, en_loop_basic/3]).
-export([de_uint8/1, en_uint8/1]).
-export([de_uint16/1, en_uint16/1]).
-export([de_uint32/1, en_uint32/1]).

%% @doc 对数据包进行解压
de_packet(<<Section:?SECTION_SIZE, Method:?METHOD_SIZE, Seq:16, Rest/binary>>) ->
    % 根据协议中的section和method属性获取对应的模块和函数.
    % 序列号也作为msgid一部分
    MsgId = {Section, Method, Seq},
    {Mod, Proto, Fun} = ?PROTO_CONVERT(MsgId),
    {ok, Req} = Proto:Fun(Rest),
    {ok, MsgId, {Mod, Fun}, Req};
de_packet(_Packet) ->
    error(packet_broken).

%% @doc 对消息进行编码,返回iodata
en_packet({Section, Method, Seq}, Record) ->
    % 消息编码成二进制
    Mod = ?PROTO_CONVERT(Section),
    Bin = Mod:encode_s2c(Record),
    Size = iolist_size(Bin) + 4,
    ?ASSERT(Size =< 16#ffff - 4),
    {[<<Size:16, Section:?SECTION_SIZE, Method:?METHOD_SIZE, Seq:16>>, Bin], Size}.

%% @doc 对字符串进行解压
de_string(<<>>) ->
    {<<>>, <<>>};
de_string(<<Len:16, Str:Len/bytes, Rest/bytes>>) ->
    {Str, Rest}.

%% @doc 对字符串进行编码
en_string(Str) when is_list(Str) ->
    Len = iolist_size(Str),
    ?ASSERT(Len =< 16#ffff),
    <<Len:16, ?S2B(Str)/bytes>>;
en_string(Str) when is_binary(Str) ->
    Len = byte_size(Str),
    ?ASSERT(Len =< 16#7fff),
    <<Len:16, Str/bytes>>.

%% @doc 对循环进行解码,参看proto_map:move/1函数
de_loop(<<Loop:?LOOP_SIZE, Rest/bytes>>, Fun, Label) ->
    case de_loop(Rest, Fun, Loop, Loop, []) of
        {ok, N, L, Bin} ->
            {ok, N, L, Bin};
        _ ->
            error({proto, de_loop, Label})
    end;
de_loop(_, _Fun, Label) ->
    error({proto, invalid_loop, Label}).

de_loop(Bin, _Fun, 0, N, Acc) ->
    {ok, N, lists:reverse(Acc), Bin};
de_loop(Bin, Fun, Cur, N, Acc) ->
    case catch Fun(Bin) of
        {'EXIT', _Reason} ->
            {error, loop_fun};
        {Obj, Rest} ->
            Acc2 = [Obj | Acc],
            de_loop(Rest, Fun, Cur-1, N, Acc2)
    end.

%% @doc 将列表打包
en_loop(List, Fun, Label) ->
    en_loop(List, length(List), Fun, Label).

en_loop(List, N, Fun, Label) ->
    try
        Part = lists:map(Fun, List), 
        [<<N:?LOOP_SIZE>> | Part]
    catch
        Class:_Reason ->
            ?ERROR(?_U("en_loop:~p错误~p:~p"), [Label, Class, _Reason]),
            error({proto, en_loop, Label})
    end.

%% @doc 对基础数据类型进行打包
en_loop_basic(List, Type, Label) ->
    try
        en_loop_basic(List, Type)
    catch
        Class:_Reason ->
            ?ERROR(?_U("en_loop:~p错误~p:~p"), [Label, Class, _Reason]),
            error({proto, en_loop, Label})
    end.

en_loop_basic(List, uint8) ->
    [<<(length(List)):?LOOP_SIZE>>, List];
en_loop_basic(List, uint16) ->
    [<<(length(List)):?LOOP_SIZE>>, [<<N:16>> || N <- List]];
en_loop_basic(List, uint32) ->
    [<<(length(List)):?LOOP_SIZE>>, [<<N:32>> || N <- List]];
en_loop_basic(List, string) ->
    [<<(length(List)):?LOOP_SIZE>>, [en_string(Str) || Str <- List]].

%% @doc uint8解码
de_uint8(<<Obj:8, Rest/binary>>) ->
    {Obj, Rest}.

en_uint8(Obj) ->
    <<Obj:8>>.

de_uint16(<<Obj:16, Rest/binary>>) ->
    {Obj, Rest}.

en_uint16(Obj) ->
    <<Obj:16>>.

de_uint32(<<Obj:32, Rest/binary>>)->
    {Obj, Rest}.

en_uint32(Obj) ->
    <<Obj:32>>.

%%
%% EUNIT test
%%
-ifdef(EUNIT).

de_string_test_() ->
    [
    ?_assertEqual({<<>>, <<>>}, de_string(<<>>)),
    ?_assertEqual({<<"hello">>, <<>>}, de_string(<<5:16, "hello">>)),
    ?_assertEqual({<<"hello">>, <<"world">>}, de_string(<<5:16, "hello", "world">>)),
    ?_assertError(function_clause, de_string("hello"))
    ].

en_string_test() ->
    ?assertEqual(<<3:16, "yes">>, en_string("yes")),
    ?assertEqual(<<3:16, "yes">>, en_string(<<"yes">>)),
    ?assertError({assert, _}, en_string(<<56:(8*256)/unit:256>>)),
    ok.

de_loop_test() ->
    F = 
    fun(<<X, Rest/bytes>>) ->
            {X, Rest}
    end,
    Bin = <<2:?LOOP_SIZE, 1, 2>>,
    ?assertEqual({ok, 2, [1, 2], <<>>},
        de_loop(Bin, F, "test")),
    ?assertEqual({ok, 2, [1, 2], <<3>>},
        de_loop(<<Bin/binary, 3>>, F, "test")),
    ok.

-endif.
