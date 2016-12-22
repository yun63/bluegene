%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2011.04.21
%%% @doc the crontab record defines
%%%
%%%----------------------------------------------------------------------

-define(CRON_ANY,   1). % "*"
-define(CRON_NUM,   2). % 2
-define(CRON_RANGE, 4). % 2-3
-define(CRON_LIST,  8). % "2,3-6"
-type cronf_type() :: 1 | 2 | 4 | 8.

-type cronf_num() :: non_neg_integer().
-type cronf_range() :: {non_neg_integer(), non_neg_integer(), pos_integer()}.
-type cronf_list() :: [cronf_num() | cronf_range()].

-type cronf_value() :: cronf_num() | cronf_range() | cronf_list().

-record(cron_entry, {
        m,       % minute
        h,       % hour
        dom,     % day of month
        mon,     % month
        dow,     % day of week
        mfa      % the mfa
   }).
-type cron_entry() :: #cron_entry{}.
