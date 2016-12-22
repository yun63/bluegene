%%%----------------------------------------------------------------------
%%%
%%% @author BigBlackBear
%%% @date  2010.12.27
%%% @doc  对gm_command的封装，加入了对特殊帐号和特殊操作的处理
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(gm).
-include("wg.hrl").
-include("game.hrl").

-export([command_process/2]).

%% @doc 处理gm命令
command_process(Player, Content) ->
    IsGm = player_state:is_gm(Player),
    case (IsGm) of
        true ->
            case catch gm_command:process(Player, Content) of
                {ok, Player2} ->
                    game_log:gm_command(Player, ?B2S(Content)),
                    {ok, Player2};
                Other ->
                    Other
            end;
        false ->
            false
    end.
