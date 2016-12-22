%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng@gmail.com
%%% @doc the utility module 
%%%
%%%----------------------------------------------------------------------
-module(wg_util).
-author('songze.me@gmail.com').
-vsn('0.1').
-include("wg_internal.hrl").
-include("const.hrl").
-include_lib("kernel/include/file.hrl").

-export([atom_to_binary/1, binary_to_atom/1]).
-export([get_app_vsn/0]).
-export([any_to_list/1, any_to_binary/1, any_to_iodata/1]).
-export([universal_data/0, universal_time/0]).
-export([now/0, now_sec/0, now_ms/0, day_sec/1, hour_sec/1,
        is_time_in_one_day/1, is_time_in_one_day/2,
        min_sec/1, second_0_to_1970/0, now_sec_to_universal_time/1]).
-export([datetime_to_iso/1, datetime_to_str/1]).
-export([human_num/1, human_size/1]).
-export([throw_on_error/2]).

-export([is_exported/1]).
-export([is_app_running/1]).
%% GUID
-export([guid/0, guid/1, guid_str/1, md5_string/1]).
%% rand
-export([rand/0, rand/2, rand/3]).

%% string和erlang term之间的转化
-export([string_to_term/1, term_to_string/1]).

%% 关于网络
-export([peer/1, peer_str/1, get_nonlo_if/0, ip_ntoa/1, ip_aton/1, ipv4_to_n/1]).
-export([get_ip/0, get_ip_wan/0, get_ip_lan/0]).
-export([recv_all_msg/0]).
-export([get_socket_opts/1, get_port_info/1]).

%% 关于文件
-export([consult_nested/1, is_dir/1, is_regular/1, is_file/1, read_file_info/1]).

%% 关于系统
-export([system_info/0, backtrace/0, backtrace/1]).
-export([tc/2, tc/4]).
-export([msg_queue_len/0, msg_queue_len/1]).

%% @doc convert atom to binary
-spec atom_to_binary(Atom :: atom()) -> binary().
atom_to_binary(Atom) ->
    <<131, 100, Len:2/unit:8, Bin:Len/binary>> = term_to_binary(Atom),
    Bin.

%% @doc convert binary to atom
-spec binary_to_atom(Bin :: binary()) -> atom().
binary_to_atom(Bin) ->
    Len = byte_size(Bin),    
    binary_to_term(<<131, 100, Len:2/unit:8, Bin/binary>>).

%% @doc any convert to list
-spec any_to_list(Any :: any()) -> list().
any_to_list(Any) when is_list(Any) -> 
    Any;
any_to_list(Any) when is_binary(Any) -> 
    binary_to_list(Any);
any_to_list(Any) when is_atom(Any) -> 
    atom_to_list(Any);
any_to_list(Any) when is_integer(Any) -> 
    integer_to_list(Any);
any_to_list(Any) when is_float(Any) -> 
    float_to_list(Any).

%% @doc convert any to binary
-spec any_to_binary(Any :: any()) -> binary().
any_to_binary(Any) when is_binary(Any) ->
    Any;
any_to_binary(Any) ->
    list_to_binary(any_to_list(Any)).

%% @doc 转化成iodata
any_to_iodata(Any) when is_list(Any) -> 
    Any;
any_to_iodata(Any) when is_binary(Any) -> 
    Any;
any_to_iodata(Any) when is_atom(Any) -> 
    atom_to_list(Any);
any_to_iodata(Any) when is_integer(Any) -> 
    integer_to_list(Any);
any_to_iodata(Any) when is_float(Any) -> 
    float_to_list(Any).

%% @doc get the application vsn
-spec get_app_vsn() -> 'undefined' | string().
get_app_vsn() ->
    case application:get_application() of
        {ok, App} ->
            case application:get_key(App, vsn) of
                {ok, Vsn} ->
                    Vsn;
                undefined ->
                    undefined
            end;
        undefined ->
            undefined
    end.

%% @doc get the universal date exclude the time
universal_data() ->
    element(1, erlang:universaltime()).

%% @doc get the universal time exclude the date
universal_time() ->
    element(2, erlang:universaltime()).

%% @doc 获取当前时间
now() ->
    case wg_time_cache:now() of
        ?NONE ->
            erlang:now();
        V ->
            V
    end.

%% @doc return the now in seconds
-spec now_sec() -> pos_integer().
now_sec() ->
    case wg_time_cache:now_sec() of
        ?NONE ->
            {Mega, Sec, _Micro} = erlang:now(),
            Mega * 1000000 + Sec;
        V ->
            V
    end.

%% @doc return the now timestamp in milliseconds
-spec now_ms() -> pos_integer().
now_ms() ->
    case wg_time_cache:now_ms() of
        ?NONE ->
            {Mega, Sec, Micro} = erlang:now(),
            (Mega * 1000000 + Sec) * 1000 + (Micro div 1000);
        V ->
            V
    end.

%% @doc convert day to seconds
-spec day_sec(Day :: integer()) -> integer().
day_sec(Day) -> Day * 86400.

%% @doc convert hour to seconds
-spec hour_sec(Hour :: integer()) -> integer().
hour_sec(Hour) -> Hour * 3600.

%% @doc convert minitue to seconds
-spec min_sec(Min :: integer()) -> integer().
min_sec(Min) -> Min * 60.

%% @doc 0年到1970之间的秒数
second_0_to_1970() ->
    62167219200. % 719528 * 86400.

%% @doc 距离1970年秒,转换成datetime
now_sec_to_universal_time(Sec) ->
    calendar:gregorian_seconds_to_datetime(Sec + second_0_to_1970()).

%% 判断某个时间是否为当天
is_time_in_one_day(Time) ->
    Now = wg_util:now_sec(),
    (Time div 86400) =:= (Now div 86400). %以零点为分界线

%% @doc 判断两个时间是否为同一天
is_time_in_one_day(T1, T2) ->
    (T1 div 86400) =:= (T2 div 86400). %以零点为分界线

%% @doc time to iso str: 20090827T04:43:14
datetime_to_iso({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    lists:flatten(
        io_lib:format("~4..0B~2..0B~2..0BT~2..0B:~2..0B:~2..0B",
                    [Year, Month, Day, Hour, Minute, Second])).

%% @doc time to iso str: 2009-08-27 04:43:14
datetime_to_str({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    lists:flatten(
        io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                    [Year, Month, Day, Hour, Minute, Second])).

-define(CLASSN, {"", " K", " M", " G", " T"}).
%% @doc convert number to human readable format, for number
-spec human_num(N :: non_neg_integer()) -> string().
human_num(N) when is_integer(N) ->
    np0(N, 0, 1, ?CLASSN).

-define(CLASSS, {"", " KB", " MB", " GB", " TB"}).
%% @doc convert number to human readable format, for size
-spec human_size(N :: non_neg_integer()) -> string().
human_size(N) when is_integer(N) ->
    np0(N, 0, 1, ?CLASSS).

%% number print
np0(N, R, L, Class) when N >= 1024 ->
    np0(N div 1024, (N rem 1024) * round(math:pow(1024, L-1)) + R, L+1, Class);
np0(N, 0, L, Class) ->
    lists:concat([N, element(L, Class)]);
np0(N, R, L, Class) ->
    lists:flatten(
        io_lib:format("~b.~3..0b~s", [N, round(R * 1000 / math:pow(1024, L-1)), 
                element(L, Class)])
    ).

%% @doc run the Thunk, throw on error
throw_on_error(E, Thunk) ->
    case Thunk() of
        {error, Reason} -> throw({E, Reason});
        {ok, Res}       -> Res;
        Res             -> Res 
    end.

%% @doc check if the function is exported
is_exported({M, F, Arity}) ->
    Exports = M:module_info(exports),
    case lists:keyfind(F, 1, Exports) of
        {F, Arity} ->
            true;
        _ ->
            false
    end.

%% 某个app是否运行中
is_app_running(Apps) when is_list(Apps) ->
    lists:all(fun is_app_running/1, Apps);
is_app_running(App) when is_atom(App) ->
    case lists:keyfind(App, 1, application:which_applications()) of
        false ->
            false;
        _ -> 
            true
    end.

%% @doc 生成guid
guid() ->
    guid(undefined).

guid(undefined) ->
    erlang:md5(term_to_binary({self(), erlang:now(), make_ref()}));
guid(Id) when is_integer(Id) ->
    <<_:32, Rest/bytes>> = guid(undefined),
    <<Id:32, Rest/bytes>>.

%% @doc 将GUID以string方式显示
%% 比如: 3F2504E0-4F89-11D3-9A0C-0305E82C3301 
%% 4-2-2-2-6
guid_str(GUID) when is_binary(GUID), byte_size(GUID) =:= 16 ->
    Str = mochihex:to_hex(binary_to_list(GUID)),
    {P1, Rest1} = lists:split(8, Str),
    {P2, Rest2} = lists:split(4, Rest1),
    {P3, Rest3} = lists:split(4, Rest2),
    {P4, Rest4} = lists:split(4, Rest3),
    {P5, []} = lists:split(12, Rest4),
    lists:concat([P1, "-", P2, "-", P3, "-", P4, "-", P5]).

%% @doc 生成md5 16进制字符串
md5_string(Bin) ->
    <<N:128>> = erlang:md5(Bin),
    lists:flatten(io_lib:format("~32.16.0b", [N])).

rand() ->
    {_,A2,A3} = erlang:now(),
    A1 = random:uniform(65535),
    random:seed(A1, A2, A3),
    random:uniform().

rand(Min,Min,N) when 0<N ->
    lists:duplicate(N, Min);
rand(Min,Max,N) when Min < Max, 0<N ->
    rand_r(Min,Max,N,[]).

rand_r(_Min,_Max,0,Ret) ->
    Ret;
rand_r(Min,Max,N,Ret) ->
    rand_r(Min,Max,N-1,[rand(Min,Max)|Ret]).

rand(Max,Max)->
    Max;
rand(Min,Max) ->
    try
        crypto:start(),
        crypto:rand_uniform(Min, Max+1)
    catch
        _ ->
        0
    end.

%% @doc 将term转化为string
%% 如[1, 2]转化为"[1,2]"
term_to_string(Term) ->
    lists:flatten(io_lib:format("~w", [Term])).

%% 将string转化为term
%% 如"[1,2]"转化成[1,2]
string_to_term(Str) when is_binary(Str) ->
    string_to_term(?B2S(Str));
string_to_term(Str) when is_list(Str) ->
    case erl_scan:string(Str ++ ".") of
        {error, _, _} = Error ->
            Error;
        {ok, Tokens, _} ->
            erl_parse:parse_term(Tokens)
    end.

%%------------
%% 网络相关
%%------------
%% @doc 获取socket peer的ip
peer_str(Sock) when is_port(Sock) ->
    inet_parse:ntoa(peer(Sock)).

%% @doc 获取socket peer的ip
peer(Sock) when is_port(Sock) ->
    case inet:peername(Sock) of
        {ok, {Addr, _Port}} ->
            Addr;
        {error, Error} ->
            throw({error, Error})
    end.

%% @doc选择非lo接口 
get_nonlo_if()->
    {ok, List} =inet:getif(),
    Set=
    lists:foldl(
    fun
        ({{127, _, _, _}, _, _}, Acc)->
            Acc;
        ({Ip, _, _}, Acc)->
            sets:add_element(Ip,Acc)
    end, sets:new(), List),
    sets:to_list(Set).

%% @doc ip tuple转换成string
ip_ntoa(Ip) ->
    inet_parse:ntoa(Ip).

%% @doc ip string转化成tuple
ip_aton(Ip) ->
    {ok, Addr} = inet_parse:address(Ip),
    Addr.

%% @doc 将ipv4转化成int
ipv4_to_n({A, B, C, D}) ->
    <<N:32>> = <<A, B, C, D>>,
    N;
ipv4_to_n(Ip) when is_list(Ip) ->
    Addr = ip_aton(Ip),
    ipv4_to_n(Addr).

%% @doc 获取本级的所有ip列表
get_ip() ->
    {ok, L} = inet:getif(),
    [Ip || {Ip, _, _} <- L, Ip =/= {127,0,0.1}].

%% @doc 获取外网ip
get_ip_wan() ->
    [Ip || {A, B, _, _} = Ip <- get_ip(), 
        Ip =/= {127,0,0,1}, (A =/= 192 andalso B =/= 168)].

%% @doc 获取内网ip
get_ip_lan() ->
    [Ip || {192, 168, _, _} = Ip <- get_ip()].

%% @doc 接收所有消息
recv_all_msg() ->
    lists:reverse(do_recv_all_msg([])).
do_recv_all_msg(Acc) ->
    receive 
        Msg ->
            do_recv_all_msg([Msg | Acc])
    after 
        0 ->
            Acc
    end.

%% @doc 获取socket选项
get_socket_opts(Socket) ->
    Opts = [active, delay_send, exit_on_close, header, keepalive, nodelay, packet, packet_size, recbuf, reuseaddr, send_timeout, send_timeout_close, sndbuf, priority, tos],
    inet:getopts(Socket, Opts).

%% @doc 获取port信息
get_port_info(Port) ->
    Info = erlang:port_info(Port),
    Input = ?PLIST_VAL(input, Info),
    OutPut = ?PLIST_VAL(output, Info),
    Connected = ?PLIST_VAL(connected, Info),
    % send_pend表示待发送的字节(位于inet_drv的queue中)
    Info2 =
    case inet:getstat(Port, [recv_cnt, recv_oct, send_cnt, send_oct, send_pend]) of
        {error, _} ->
            [];
        {ok, Values} ->
            Values
    end,
    [{input, Input}, {output, OutPut}, {connected, Connected}] ++ Info2.

%%----------
%% 关于文件
%%----------

%% @doc 嵌套的解析erlang term文件
consult_nested(File) ->
    case catch consult_nested(File, [], []) of
        {error, _} = Ret ->
            Ret;
        {Terms, _} ->
            %?WARN(?_U("************* terms:~n~p~n"), [Terms]),
            {ok, Terms}
    end.
consult_nested(FileName, Terms, Already) ->
    Config =
    case file:consult(FileName) of
        {ok, L} ->
            L;
        Error ->
            ?ERROR("the ~p file format error!~n", [FileName]),
            throw(Error)
    end,
    Already2 = [FileName | Already],

    % 解析每一项
    {Terms2, Already3} =
    lists:mapfoldl(
    fun
        ({include, Val}, Acc) ->
            % 子配置文件列表
            Files = do_expand_files(FileName, Val, Acc),
            %?WARN(?_U("expand files list:~p"), [Files]),
            lists:mapfoldl(
            fun(F, AccIn) ->
                consult_nested(F, [], AccIn)
            end, Already2, Files);
        (_Other, Acc) ->
            {_Other, Acc}
    end, Already2, Config),
{lists:flatten([Terms | Terms2]), Already3}.

%% 获取子配置文件
do_expand_files(Filename, Val0, Already) ->
    DirName = filename:dirname(Filename),
    Val = filename:join([DirName, Val0]),
    case lists:member($*, Val) of
        true ->
            lists:foldl(
            fun(F, Acc) ->
                case lists:member(F, Already) of
                    true ->
                        Acc;
                    false ->
                        [F | Acc]
                end
            end, [], filelib:wildcard(Val));
        false ->
            case lists:member(Val, Already) of
                true ->
                    [];
                false ->
                    [Val]
            end
    end.

%% @doc 是否为目录
is_dir(File) ->
    case prim_file:read_file_info(File) of
        {ok, #file_info{type=directory}} ->
            true;
        _ ->
            false
    end.

%% @doc 是否为常规文件
is_regular(File) ->
    case prim_file:read_file_info(File) of
        {ok, #file_info{type=regular}} ->
            true;
        _ ->
            false
    end.

%% @doc 是否为文件或目录(同filelib:is_file/1)
is_file(File) ->
    case prim_file:read_file_info(File) of
        {ok, #file_info{type=regular}} ->
            true;
        {ok, #file_info{type=directory}} ->
            true;
        _ ->
            false
    end.

%% @doc 读取文件信息
read_file_info(File) ->
    prim_file:read_file_info(File).

%%----------
%% 关于系统
%%----------

%% @doc 显示系统信息
system_info() ->
    List = [build_type, debug_compiled, dist_buf_busy_limit, global_heaps_size, 
        heap_type, kernel_poll, logical_processors,logical_processors_online,
        scheduler_bind_type, scheduler_bindings,
        multi_scheduling, process_count, process_limit, smp_support, wordsize],
    [{Type, erlang:system_info(Type)} || Type <- List] ++
    [erlang:system_info(fullsweep_after)] ++
    [{list_to_atom(lists:concat(["check_io_", Key])), Val}
        || {Key, Val} <- erlang:system_info(check_io), Key =:= max_fds] ++
    [{context_switches, erlang:statistics(context_switches)},
    {garbage_collection, erlang:statistics(garbage_collection)},
    {io, erlang:statistics(io)},
    {reductions, erlang:statistics(reductions)},
    {run_queue, erlang:statistics(run_queue)},
    {runtime, erlang:statistics(runtime)},
    {wall_clock, erlang:statistics(wall_clock)}].

%% @doc 进程的backtrace
backtrace() ->
    backtrace(self()).

%% @doc 进程的backtrace
backtrace(Pid) ->
    {_, Info} = erlang:process_info(Pid, backtrace),
    Info.

%% @doc 统计某个函数的平均耗时
tc(Fun, Times) when is_function(Fun, 0), Times > 0 ->
    TList =
    [begin
        {T, _} = timer:tc(Fun, []),
        T
    end || _ <- lists:seq(1, Times)],
    lists:sum(TList) / Times.

%% @doc 统计某个函数的平均耗时
tc(M, F, A, Times) when Times > 0 ->
    TList =
    [begin
        {T, _} = timer:tc(M, F, A),
        T
    end || _ <- lists:seq(1, Times)],
    lists:sum(TList) / Times.

%% @doc 获取进程的消息队列长度
msg_queue_len() ->
    msg_queue_len(self()).
msg_queue_len(Pid) ->
    case erlang:process_info(Pid, message_queue_len) of
        {message_queue_len, Len} ->
            Len;
        Other ->
            Other
    end.

%%------------
%% EUnit TEST
%%------------

-ifdef(EUNIT).
atom_test_() ->
    [
        ?_assertEqual(<<"ok">>, atom_to_binary(ok)),
        ?_assertEqual(<<"OK">>, atom_to_binary('OK')),
        ?_assertEqual(ok, binary_to_atom(<<"ok">>)),
        ?_assertEqual('OK', binary_to_atom(<<"OK">>))
    ].

any2list_test_() ->
    [
        ?_assertEqual("myname", any_to_list(myname)),
        ?_assertEqual("234", any_to_list(234)),
        ?_assertEqual("234", any_to_list(<<"234">>)),
        ?_assertEqual("234", any_to_list("234"))
    ].

any2binary_test_() ->
    [
        ?_assertEqual(<<"myname">>, any_to_binary(myname)),
        ?_assertEqual(<<"234">>, any_to_binary(234)),
        ?_assertEqual(<<"234">>, any_to_binary(<<"234">>)),
        ?_assertEqual(<<"234">>, any_to_binary("234"))
    ].

now_sec_test_() ->
    [
        ?_assertEqual(now_sec(), now_ms() div 1000)
    ].

np_test_() ->
    [
        ?_assertEqual("0", human_size(0)),
        ?_assertEqual("1023", human_size(1023)),
        ?_assertEqual("1 KB", human_size(1024)),
        ?_assertEqual("1.001 KB", human_size(1025)),
        ?_assertEqual("1.999 KB", human_size(2047)),
        ?_assertEqual("1 MB", human_size(1048576)),
        ?_assertEqual("1023.999 MB", human_size(1073740800)),
        ?_assertEqual("23.120 GB", human_size(24824910971))
    ].

time_test_() ->
    [
        ?_assertEqual(60 * 20, min_sec(20)), 
        ?_assertEqual(3600 * 20, hour_sec(20)), 
        ?_assertEqual(86400 * 20, day_sec(20)), 
        ?_assertEqual(0, day_sec(0)), 
        ?_assertEqual(0, hour_sec(0)), 
        ?_assertEqual(0, min_sec(0)) 
    ].

throw_on_error_test() ->
    ?assertEqual(ok, throw_on_error(dummy, fun() -> ok end)),
    ?assertEqual(yes, throw_on_error(dummy, fun() -> {ok, yes} end)),
    ?assertThrow({type, some_error}, throw_on_error(type, fun() -> {error, some_error} end)),
    ok.

is_exported_test() ->
    ?assertEqual(true, is_exported({?MODULE, get_app_vsn, 0})),
    ?assertEqual(false, is_exported({?MODULE, get_app_vsn, 1})),
    ok.

guid_test_() ->
    [
    ?_assertMatch(<<_:128>>, guid()),
    ?_assertMatch(<<1:32, _:96>>, guid(1)),
    ?_assertEqual("00010203-0405-0607-0809-0A0B0C0D0E0F", 
        string:to_upper(guid_str(<<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15>>)))
    ].

term_test_() ->
    [
        ?_assertEqual("[]", term_to_string([])),
        ?_assertEqual("[1,2]", term_to_string([1,2])),
        ?_assertEqual("[1,2,atom]", term_to_string([1, 2, atom])),
        ?_assertEqual({ok, [1,2]}, string_to_term("[1,2]")),
        ?_assertEqual({ok, [1,2,atom]}, string_to_term("[1,2,atom]"))
    ].

-endif.
