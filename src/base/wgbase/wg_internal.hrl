%%%----------------------------------------------------------------------
%%%
%%% @copyright wg
%%%
%%% @author songze.me@gmail.com
%%% @doc wg header file
%%%
%%%----------------------------------------------------------------------
-ifndef(WG_INTERNAL_HRL).
-define(WG_INTERNAL_HRL, ).
-include_lib("kernel/include/inet.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("wg.hrl").
-include("wg_log.hrl").

%% some type defines
-type dirname() :: string().


-endif. % WG_INTERNAL_HRL
