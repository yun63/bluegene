%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2010.11.04
%%% @doc 进行数据的加密解密
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(gate_crypt).
-author("litaocheng@gmail.com").
-vsn('0.1').
-include("wg.hrl").

-export([encrypt/1]).
-export([decrypt/1]).

%% @doc 数据加密 FIXME
encrypt(Bin) ->
    Bin.

%% @doc 数据解密 FIXME
decrypt(Bin) ->
    Bin.


%%
%% EUNIT test
%%
-ifdef(EUNIT).


-endif.
