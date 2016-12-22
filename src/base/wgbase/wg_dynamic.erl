%%%----------------------------------------------------------------------
%%%
%%% @author BigBlackBear
%%% @doc 动态模块的支持
%%%
%%%----------------------------------------------------------------------
-module(wg_dynamic).

-export([compile_and_load/2]).

compile_and_load(Fn, Src) ->
    Dir = tempdir(),
    Fname = filename:join(Dir, Fn),
    {ok, Fd} = file:open(Fname, [read, write]),
    file:write(Fd, Src),
    file:position(Fd, 0),
    case epp_dodger:parse(Fd) of
    {ok, Tree} ->
        Forms = revert_tree(Tree),
        file:close(Fd),
        file:delete(Fname),
        ok = load_file(Fn, Forms);
    Error ->
        file:close(Fd),
        file:delete(Fname),
        Error
    end.

revert_tree(Tree) ->
    [erl_syntax:revert(T) || T <- Tree].

load_file(Fn, Forms) ->
    case compile:forms(Forms, [binary]) of
        {ok, M, Bin} ->
            {module, _} = code:load_binary(M, Fn, Bin),
            ok;
        Error ->
            {error, Error}
    end.

tempdir() ->
    case os:getenv("TMPDIR") of
        false ->
            "/tmp";
        Value ->
            Value
    end.
