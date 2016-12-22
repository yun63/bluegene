%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @author songze <songze.me@gmail.com>
%%% @doc the cron server, run the periodic task
%%%
%%%----------------------------------------------------------------------
-module(wg_crontab_server).
-author('songze.me@gmail.com').
-vsn('0.1').
-behaviour(gen_server).
-include("wg_internal.hrl").
-include("wg_crontab.hrl").

-export([start_link/1, start_link/2]).
-export([i/0, i/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).


-record(state, {
        file = "" :: string(),          % file name
        mtime,                          % last modify time
        entrys = [] :: [cron_entry()],  % the cron tasks
        file_timer :: reference(),      % the check file last modified timer
        cron_timer :: reference()       % the check cron task timer
    }).

-define(CHECK_FILE_INTERVAL, 60000). % 1 min
-define(CHECK_CRON_INTERVAL, 60000). % 1 min
        
%% @doc start the cron server
start_link(File)  ->
    start_link(?MODULE, File).

start_link(Name, File) when is_atom(Name) ->
    ?DEBUG(?_U("启动crontab服务:~p 对应文件:~p"), [Name, File]),
    gen_server:start_link({local, Name}, ?MODULE, File, []).

%% @doc show info
i() ->
    i(?MODULE).
i(Name) ->
    case catch gen_server:call(Name, get_state) of
        #state{mtime = MTime, entrys = Entrys} ->
            [{mtime , MTime}, {entrys, Entrys}];
        _ ->
            []
    end.

%% gen_server callbacks
init(File) ->
    process_flag(trap_exit, true),
    case parse(File) of
        {ok, Entrys} ->
            State = #state{
                file = File,
                mtime = filelib:last_modified(File),
                entrys = Entrys,
                file_timer = check_file_timer(),
                cron_timer = check_cron_timer()
            },
            {ok, State};
        Error ->
            ?DEBUG("error :~p", [Error]),
            Error
    end.

handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _Ref, check_file}, State = #state{file = File, mtime = MTime}) ->
    %?DEBUG("check the file :~p", [State]),
    State2 = State#state{
        file_timer = check_file_timer()
    },
    MTimeNew = filelib:last_modified(File),
    case  MTimeNew > MTime of
        true -> % reload crontab
            case parse(File) of
                {ok, Entrys} ->
                    State3 = State2#state{
                        file = File,
                        mtime = MTimeNew,
                        entrys = Entrys
                    },
                    {noreply, State3};
                _Error ->
                    ?WARN("the crontab file ~s format error:~p", [File, _Error]),
                    {noreply, State2}
            end;
        false ->
            {noreply, State2}
    end;
handle_info({timeout, _Ref, check_cron}, State = #state{entrys = Entrys}) ->
    %?DEBUG("check the cron :~p", [State]),
    State2 = State#state{
        file_timer = check_cron_timer()
    },
    check_entrys(Entrys),
    {noreply, State2};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
    
%%-----------------------------------------------------------------------------
%%
%% internal API
%%
%%-----------------------------------------------------------------------------

%% start the check file timer
check_file_timer() ->
    erlang:start_timer(?CHECK_FILE_INTERVAL, self(), check_file).

%% start the cron tasks timer
check_cron_timer() ->
    erlang:start_timer(?CHECK_CRON_INTERVAL, self(), check_cron).


%% check the cron entrys
check_entrys(Entrys) ->
    Now = {Date, _Time} = erlang:localtime(), 
    Week = calendar:day_of_the_week(Date),
    lists:foreach(
        fun(Entry) ->
                case can_run(Entry, Now, Week) of
                    true ->
                        %?DEBUG("run this task:~p", [Entry]),
                        run_task(Entry#cron_entry.mfa);
                    false ->
                        %?DEBUG("can't run this task:~p", [Entry]),
                        ok
                end
        end,
        Entrys).

can_run(Entry, {{_, CurMon, CurDay}, {CurH, CurM, _}}, Week) ->
    #cron_entry{
        m = M,
        h = H,
        dom = Dom,
        mon = Mon,
        dow = Dow
    } = Entry,
    field_ok(M, CurM) andalso
    field_ok(H, CurH) andalso
    field_ok(Dom, CurDay) andalso 
    field_ok(Dow, Week) andalso
    field_ok(Mon, CurMon). 

%% check if the field is ok
field_ok({?CRON_ANY, _}, _Cur) ->
    true;
field_ok({?CRON_NUM, Val}, Cur) ->
    Val =:= Cur;
field_ok({?CRON_RANGE, {First, Last, Step}}, Cur) ->
    range_ok(Cur, First, Last, Step);
field_ok({?CRON_LIST, List}, Cur) ->
    lists:any(
        fun(FInList) ->
                field_ok(FInList, Cur)
        end,
        List).

%% check if the value in the range
range_ok(Val, First, Last, Step) ->
    range_ok1(Val, First, Last, Step).

range_ok1(Val, Val, _Last, _Step) ->
    true;
range_ok1(_Val, Cur, Last, _Step) when Cur >= Last ->
    false;
range_ok1(Val, Cur, Last, Step) ->
    range_ok1(Val, Cur + Step, Last, Step).

%% run the task
run_task({M, F, A} = _Task) ->
    %?DEBUG("run the cron task:{~p, ~p, ~p}", [M, F, A]),
    proc_lib:spawn(
        fun() ->
            case catch apply(M, F, A) of
                {'EXIT', R} ->
                    ?ERROR(?_U("定时任务:~p 执行失败: ~p"), [_Task, R]),
                    ok;
                _ ->
                    ok
            end
        end
    ).

%%------------
%% 解析文件
%%------------

%% parse the crontab config file
parse(File) ->
    case wg_util:consult_nested(File) of
        {error, enoent} = Error ->
            ?WARN(?_U("crontab file ~s 不存在"), [File]),
            Error;
        {error, R} = Error ->
            ?WARN(?_U("crontab file 格式错误: ~p"), [file:format_error(R)]),
            Error;
        {ok, CronTab} ->
            parse_entrys(CronTab)
    end.

%% parse all the entrys
parse_entrys(CronTab) ->
    Entrys =
    lists:foldl(
        fun(Entry, Acc) ->
                case catch parse_entry(Entry) of
                    {ok, CronEntry} ->
                        [CronEntry | Acc];
                    {error, R} ->
                        ?WARN("the line :~p error:~p", [Entry, R]),
                        Acc
                end
        end,
        [],
        CronTab),
    {ok, Entrys}.

%% parse the single entry
parse_entry({"@yearly", {Mod, F, A} = MFA}) when is_atom(Mod), is_atom(F), is_list(A) ->
    {ok, #cron_entry{
            m = {?CRON_NUM, 0},
            h = {?CRON_NUM, 0},
            dom  = {?CRON_NUM, 1},
            mon = {?CRON_NUM, 1},
            dow = {?CRON_ANY, any},
            mfa = MFA
        }};
parse_entry({"@monthly", {Mod, F, A} = MFA}) when is_atom(Mod), is_atom(F), is_list(A) ->
    {ok, #cron_entry{
            m = {?CRON_NUM, 0},
            h = {?CRON_NUM, 0},
            dom  = {?CRON_NUM, 1},
            mon = {?CRON_ANY, any},
            dow = {?CRON_ANY, any},
            mfa = MFA
        }};
parse_entry({"@weekly", {Mod, F, A} = MFA}) when is_atom(Mod), is_atom(F), is_list(A) ->
    {ok, #cron_entry{
            m = {?CRON_NUM, 0},
            h = {?CRON_NUM, 0},
            dom  = {?CRON_ANY, any},
            mon = {?CRON_ANY, any},
            dow = {?CRON_NUM, 0},
            mfa = MFA
        }};
parse_entry({"@daily", {Mod, F, A} = MFA}) when is_atom(Mod), is_atom(F), is_list(A) ->
    {ok, #cron_entry{
            m = {?CRON_NUM, 0},
            h = {?CRON_NUM, 0},
            dom  = {?CRON_ANY, any},
            mon = {?CRON_ANY, any},
            dow = {?CRON_ANY, any},
            mfa = MFA
        }};
parse_entry({"@hourly", {Mod, F, A} = MFA}) when is_atom(Mod), is_atom(F), is_list(A) ->
    {ok, #cron_entry{
            m = {?CRON_NUM, 0},
            h = {?CRON_ANY, any},
            dom  = {?CRON_ANY, any},
            mon = {?CRON_ANY, any},
            dow = {?CRON_ANY, any},
            mfa = MFA
        }};
parse_entry({{M, H, Dom, Mon, Dow}, {Mod, F, A} = MFA}) when is_atom(Mod), is_atom(F), is_list(A) ->
    Cron = #cron_entry{
        m = parse_field(M, 0, 59, {error, emin}),
        h = parse_field(H, 0, 23, {error, ehour}),
        dom = parse_field(Dom, 1, 31, {error, edom}),
        mon = parse_field(Mon, 1, 12, {error, emon}),
        dow = parse_field(Dow, 0, 7, {error, edow}),
        mfa = MFA
    },
    {ok, Cron}; 
parse_entry(_) ->
    {error, eformat}.


%% parset the filed
parse_field(F, Min, Max, Error) ->
    try
        parse_field(F, Min, Max)
    catch 
        Class:Type ->
            ?ERROR(?_U("解析字段错误~p ~p:~p"), [Error, Class, Type]),
            throw(Error)
    end. 

parse_field("*", _Min, _Max) ->
    {?CRON_ANY, any};
parse_field(F, Min, Max) when is_integer(F), F >= Min, F =< Max ->
    {?CRON_NUM, F};
parse_field(F = [_|_], Min, Max) ->
    case string:tokens(F, ",") of
        [Single] -> % is range
            parse_range_or_num(Single, Min, Max);
        [_|_] = Multi -> % is list
            {?CRON_LIST, lists:map(
                fun(E) ->
                        parse_field(E, Min, Max)
                end,
                Multi)}
    end.

%% parse the range string: "5", "2-5/2", "2-5", "*/2"
parse_range_or_num(Str, Min, Max) ->
    {RangeStr, Step} = 
    case string:tokens(Str, "/") of
        [Range] ->
            {Range, 1};
        [Range, StepStr] ->
            {Range, ?S2N(StepStr)}
    end,
    case RangeStr of
        "*" ->
            {?CRON_RANGE, {Min, Max, Step}};
        _ ->
            case string:tokens(RangeStr, "-") of
                [NStr] ->
                    N = ?S2N(NStr),
                    ?ASSERT(N >= Min andalso N =< Max),
                    {?CRON_NUM, N};
                [FirstStr, LastStr] ->
                    First = ?S2N(FirstStr),
                    Last = ?S2N(LastStr),
                    ?ASSERT(First >= Min andalso Last =< Max),
                    {?CRON_RANGE, {First, Last, Step}}
            end
    end.

%%------------
%% EUNIT Test
%%------------
-ifdef(EUNIT).

parse_field_test_() ->
    [
        ?_assertEqual({?CRON_ANY, any}, parse_field("*", 1, 59)),
        ?_assertEqual({?CRON_NUM, 1}, parse_field("1", 1, 59)),
        ?_assertError(_, parse_field("50", 1, 30)),

        ?_assertEqual({?CRON_RANGE, {1, 59, 1}}, parse_field("1-59/1", 1, 59)),
        ?_assertEqual({?CRON_RANGE, {1, 59, 1}}, parse_field("*/1", 1, 59)),
        ?_assertEqual({?CRON_RANGE, {1, 59, 1}}, parse_field("1-59", 1, 59)),
        ?_assertEqual({?CRON_RANGE, {1, 59, 2}}, parse_field("1-59/2", 1, 59)),

        ?_assertEqual({?CRON_LIST, [{?CRON_RANGE, {1, 59, 2}},
                    {?CRON_NUM, 2}]}, 
            parse_field("1-59/2,2", 1, 59)),
        ?_assertEqual({?CRON_LIST, [{?CRON_RANGE, {1, 59, 2}},
                    {?CRON_NUM, 2},
                    {?CRON_RANGE, {3, 4, 1}}]}, 
            parse_field("1-59/2,2,3-4", 1, 59))
    ].

-endif.
