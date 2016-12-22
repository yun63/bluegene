%%%----------------------------------------------------------------------
%%%
%%% @copyright wg
%%%
%%% @author songze.me@gmail.com 
%%% @doc handle the http request based on mochiweb
%%%
%%%----------------------------------------------------------------------
-module(wg_httpd).
-author('songze.me@gmail.com').
-vsn('0.1').
-include("wg_internal.hrl").
-include("wg_httpd.hrl").
-include("wg_log.hrl").

-export([start_link/3]).
-export([handle_request/2]).
-define(MAX_MOD_LEN, 255).

%% @doc start the http server
-type httpd_options() ::
    {name, string()} |
    {ip, string()} |
    {port, integer()}.
-spec start_link(Opts :: [httpd_options()], 
    HandlerDirs :: [dirname()],
    dirname()) -> any().
start_link(Opts, HandlerDirs, DocRoot) ->
    load_mods_name(HandlerDirs),
    Loop = fun(Req) -> ?MODULE:handle_request(Req, DocRoot) end,
    mochiweb_http:start([{loop, Loop} | Opts]).

%% @doc handle the http request
-spec handle_request(Req :: any(), DocRoot :: string()) -> 'ok'.
handle_request(Req, DocRoot) ->
    Method = Req:get(method), 
    Path = "/" ++ Path0 = Req:get(path),

    {Main, Minor} = Req:get(version),
    ?INFO("~s ~s HTTP ~B.~B", [Method, Path, Main, Minor]),

    %% handle the request
    Resp = 
    case get_handle_mod(convert_path(Path0)) of
        {ok, Mod} ->
            {Code, Headers, Data} =
            case catch  Mod:handle(Req, Method) of
                {'EXIT', {Error, _Where}} ->
                    ?ERROR("~p handle request error:~p where:~p", [Mod, Error, _Where]),
                    error_to_rsp(Error);
                {'EXIT', Error} ->
                    error_to_rsp(Error);
                {_Code, _Headers, _Data} = Rsp ->
                    Rsp
            end,
            %?DEBUG("code:~p Headers:~p Data:~p", [Code, Headers, Data]),
            send_respond(Req, Code, Headers, Data);
        {error, _} ->
            ?DEBUG("try send static file path:~p root:~p", [Path0, DocRoot]),
            Req:serve_file(Path0, DocRoot)
    end,

    ?INFO("response ~s - ~s ~s ~b", [Req:get(peer)
                        , Req:get(method)
                        , Path
                        , Resp:get(code)]).


%%
%% internal API
%% 

%% convert some path: "" -> index
convert_path("") ->
    "index";
convert_path(P) ->
    P.

%% acrodding th path, get the handle module
get_handle_mod(Path) ->
    case get_handle_mod(Path, ["_http"], 0) of
        {ok, ModStr} ->
            case catch erlang:list_to_existing_atom(ModStr) of
                {'EXIT', {badarg, _}} ->
                    ?DEBUG("to existing atom error", []),
                    {error, no_handle_mod};
                Mod ->
                    {ok, Mod}
            end;
        Other ->
            Other 
    end.


get_handle_mod(_Path, _Acc, AccLen) when AccLen >= ?MAX_MOD_LEN->
    {error, max_mod_len};
get_handle_mod("", Acc, _AccLen) ->
    {ok, lists:append(lists:reverse(Acc))};
get_handle_mod(Path, Acc, AccLen) ->
    {Part, Rest} =  mochiweb_util:path_split(Path),
    Acc2 = [Part, "_" | Acc],
    AccLen2 = AccLen + length(Part) + 1,
    get_handle_mod(Rest, Acc2, AccLen2).

error_to_rsp(?HTTP_BADARG) ->
    ?DEBUG("error is ~p", [?HTTP_BADARG]),
    {400, "", <<"400 bad request (badarg)">>};
error_to_rsp(_Error) ->
    ?DEBUG("error is ~p", [_Error]),
    {500, "", <<"500 Internal Error">>}.

%%  send data to client
send_respond(Req, Code, Headers, Data) ->
    %?DEBUG("respond...~p~n ~p~n ~p~n ~p~n.~n", [Req, Code, Headers, Data]),
    DefaultHeaders = 
    case proplists:get_value(?HTTP_CONTENT_TYPE, Headers) of
        undefined ->
              [{?HTTP_CONTENT_TYPE, set_content_type(Req, Headers)}];
          _ ->
              []
    end,
%    [
%        {?HTTP_CONTENT_TYPE, set_content_type(Req, Headers)},
%        {<<"Cache-Control">>, <<"must-revalidate">>}
%    ] ++ server_header(),
    Req:respond({Code, Headers ++ DefaultHeaders, Data}).

%{<<"Content-Type">>, negotiate_content_type(Req)},
set_content_type(Req, Headers) ->
    case proplists:get_value(?HTTP_CONTENT_TYPE, Headers) of
        undefined ->
            negotiate_content_type(Req);
        Type ->
            Type
    end.

negotiate_content_type(Req) ->
    %% Determine the appropriate Content-Type header for a JSON response
    %% depending on the Accept header in the request. A request that explicitly
    %% lists the correct JSON MIME type will get that type, otherwise the
    %% response will have the generic MIME type "text/plain"
    AcceptedTypes =
    case Req:get_header_value("Accept") of
        undefined       ->
            [];
        AcceptHeader    -> 
            string:tokens(AcceptHeader, ", ")
    end,
    case lists:member("application/json", AcceptedTypes) of
        true  -> "application/json";
        false -> 
            case AcceptedTypes of
                [] ->
                    "text/plain;charset=utf-8";
                [AType|_] ->
                    AType
            end
    end.

%server_header() ->
%    OTPVersion = "R" ++ integer_to_list(erlang:system_info(compat_rel)) ++ "B",
%    [{<<"Server">>, ["wg", "(Erlang OTP/", OTPVersion, ")"]}].


%% doc load all http handler mods name in ebin
load_mods_name(HandlerDirs) ->
    Self = code:which(?MODULE),
    Ebin = filename:dirname(Self),
    Dirs = [Ebin] ++ HandlerDirs ,

    Mods = 
    [
        [begin
            ModStr = filename:basename(F, ".beam"),
            list_to_atom(ModStr)
        end || F <- filelib:wildcard("_http_*.beam", Dir)]
     || Dir <- Dirs],
    
    ?INFO("http handler mods: ~p~n", [lists:append(Mods)]),
    ok.
