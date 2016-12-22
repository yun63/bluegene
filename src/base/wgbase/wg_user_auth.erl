%%%----------------------------------------------------------------------
%%%
%%% @copyright wg 2009 
%%%
%%% @author songze <songze.me@gmail.com>
%%% @doc the user auth module
%%%
%%%----------------------------------------------------------------------
-module(wg_user_auth).
-author('songze.me@gmail.com').
-vsn('0.1').
-include("wg_internal.hrl").

-export([token/3, token/5, valid/2]).

-define(IVEC, <<16#ad, 16#76, 16#1a, 16#49, 16#dc, 16#28, 16#e1, 16#31>>).

%% @doc use the  user and pwd generate user key
-spec token(User :: binary(), Pwd :: binary(), Secret :: binary()) ->
    binary().
token(User, Pwd, Secret) ->
    token(User, Pwd, <<>>, wg_util:now_sec(), Secret).

%% @doc use the  user and pwd generate user key
-spec token(User :: binary(), Pwd :: binary(), Data :: any(),
    Time :: integer(), Secret :: binary()) -> binary().
token(User, Pwd, Data, Time, Secret) when is_binary(User), is_binary(Pwd) ->
    ULen = byte_size(User),
    true = ULen =< 255,
    PLen = byte_size(Pwd),
    true = PLen =< 255,
    DataBin = to_binary(Data),
    DLen = byte_size(DataBin),
    true = DLen =< 255,

    Pad = 
    case (1 + ULen + 1 + PLen + 1 + DLen + 4) rem 8 of
        0 -> 0;
        N -> 8 - N
    end,
    
    Text1 = 
    <<ULen, User/binary, PLen, Pwd/binary, DLen, DataBin/binary, Time:32, 0:Pad/unit:8>>,
    Cipher1 = crypto:des_cbc_encrypt(Secret, ?IVEC, Text1),
    base64:encode(Cipher1).

%% @doc valid the key obtain the  user and pwd
-spec valid(Token :: binary(), Secret :: binary()) -> 
    {'ok', wg_token()} | 'error'.
valid(Token, Secret) ->
    Text1 = base64:decode(Token),
    case catch crypto:des_cbc_decrypt(Secret, ?IVEC, Text1) of
        <<ULen, User:ULen/binary, PLen, Pwd:PLen/binary, DLen, Data:DLen/binary, 
            Time:32, _Pad/binary>> ->
            {ok, #wg_token{user=User, pwd=Pwd, data=from_binary(Data), time=Time}};
        _ ->
            error
    end.

%%------------------------------------------------------------------------------
%%
%% internal API
%%
%%------------------------------------------------------------------------------

%% convert customized data to binary
to_binary(Data) ->
    term_to_binary(Data).

%% return erlang term from binary 
from_binary(Bin) ->
    binary_to_term(Bin).

-ifdef(EUNIT).
-define(KEY, <<"^|O2", 16#28, 16#31, 16#d5, 16#82>>).

basic_test_() ->
    Now = wg_util:now_sec(), 
    crypto:start(),
    [
        ?_assertMatch({ok, 
            #wg_token{
                user= <<"">>, 
                pwd= <<"">>, 
                data = <<>>}}, 
            valid(token(<<"">>, <<"">>, ?KEY), ?KEY)),

        ?_assertMatch({ok, 
            #wg_token{
                user= <<"songze">>, 
                pwd= <<"pwd">>, 
                data = <<>>}}, 
            valid(token(<<"songze">>, <<"pwd">>, ?KEY), ?KEY)),

        ?_assertEqual({ok, 
            #wg_token{
                user= <<"songze">>, 
                pwd= <<"pwd">>, 
                data = <<>>,
                time = Now}}, 
            valid(token(<<"songze">>, <<"pwd">>, <<>>, Now, ?KEY), ?KEY)),

        ?_assertEqual({ok, 
            #wg_token{
                user= <<"songze">>, 
                pwd= <<"pwd">>, 
                data = "some_data",
                time=Now}}, 
            valid(token(<<"songze">>, <<"pwd">>, "some_data", Now, ?KEY), ?KEY)),

        ?_assertEqual({ok, 
            #wg_token{
                user= <<"songze">>, 
                pwd= <<"pwd">>, 
                data = {data, 1},
                time=Now}}, 
            valid(token(<<"songze">>, <<"pwd">>, {data, 1}, Now, ?KEY), ?KEY))
    ].

-endif.
