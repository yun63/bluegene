%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date 2011-11-22
%%% @doc 邮件相关的模块(使用deps/gen_smtp)
%%% 用法:
%%%   Mail = wg_mail:new("litao@youguogame.com", "yourpasswd"),
%%%   Mail:send("litao@youguogame.com", "3730263@qq.com", 
%%%         "title", "no attachment"),
%%%   Mail:send_attachment("litao@youguogame.com", "3730263@qq.com", 
%%%         "title", "with attachment", [{"1.txt", "", ":)"}]),
%%%   Callback = fun(Ret) -> io:format("ret is ~p", [Ret]) end,
%%%   Mail:send("litao@youguogame.com", "3730063@qq.com", "title", 
%%%         "no attachment", Callback).
%%%
%%%----------------------------------------------------------------------
-module(wg_mail, [UserName, Password]).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("wg.hrl").

-ifdef(TEST).
-compile([export_all]).
-endif.
-export([send/4, send/5, send_attachment/5, send_attachment/6]).

-define(MAIL_BASE_OPTS, [{tls, if_available}, {ssl, false}, 
        {hostname, smtp_util:guess_FQDN()}, {retries, 2}]).

%% @doc 发送邮件, callback: fun(any()) -> any().
send(FromAddress, ToAddress, Subject, Body) ->
    send(FromAddress, ToAddress, Subject, Body, none).
send(FromAddress, ToAddress, Subject, Body, CallBack) ->
    MessageHeader = 
    build_header([
            {"Subject", Subject},
            {"To", ToAddress},
            {"From", FromAddress}], "text/plain"), 
    do_send(FromAddress, ToAddress, [MessageHeader, "\r\n", Body], CallBack).

%% @doc 发送邮件,包含附件
send_attachment(FromAddress, ToAddress, Subject, Body, Attachments) ->
    send_attachment(FromAddress, ToAddress, Subject, Body, Attachments, none).
send_attachment(FromAddress, ToAddress, Subject, Body, Attachments, Callback) ->
    {MimeType, MessageBody} = build_body_attachments(Body, Attachments),
    MessageHeader = 
    build_header([
            {"Subject", Subject},
            {"To", ToAddress},
            {"From", FromAddress}], MimeType), 
    do_send(FromAddress, ToAddress, [MessageHeader, "\r\n", MessageBody], Callback).


%%---------------------
%% Internal API
%%---------------------

%% 发送邮件
do_send(FromAddress, ToAddress, Body, CallBack) ->
    do_send(FromAddress, ToAddress, Body, ?MAIL_BASE_OPTS, CallBack).
do_send(FromAddress, ToAddress, Body, Options, CallBack) ->
    Email = {FromAddress, [ToAddress], Body},
    MailOptions1 = lists:keystore(username, 1, Options, {username, UserName}),
    MailOptions2 = lists:keystore(password, 1, MailOptions1, {password, Password}),
    MailOptions3 = 
    case proplists:lookup(relay, MailOptions2) of                                                     
        none ->
            [_User, Host] = string:tokens(ToAddress, "@"),
            [{relay, Host} | MailOptions2];
        _ ->
            MailOptions2
    end,          
    case CallBack of
        none ->
            gen_smtp_client:send_blocking(Email, MailOptions3);
        _ ->
            gen_smtp_client:send(Email, MailOptions3, CallBack)
    end.

%% 构建邮件头
build_header(HeaderFields, DefaultMimeType) ->
    MessageID = case proplists:get_value("Message-ID", HeaderFields) of
        undefined -> smtp_util:generate_message_id();
        Other -> Other
    end,
    ContentType = proplists:get_value("Content-Type", HeaderFields, DefaultMimeType),
    Date = proplists:get_value("Date", HeaderFields, erlydtl_dateformat:format("r")),
    BaseHeader = [
        "Date: ", Date, "\r\n",
        "Content-Type: ", ContentType, "\r\n",
        "MIME-Version: ", "1.0", "\r\n",
        "Message-ID: ", MessageID, "\r\n"],
    add_fields(["Subject", "From", "To", "Reply-To"], HeaderFields, BaseHeader).

add_fields([], _, Acc) ->
    Acc;
add_fields([Field|Rest], HeaderFields, Acc) ->
    case proplists:get_value(Field, HeaderFields) of
        undefined ->
            add_fields(Rest, HeaderFields, Acc);
        Value ->
            add_fields(Rest, HeaderFields, [Field, ": ", Value, "\r\n" | Acc])
    end.

%% 构建附件
build_body_attachments(Body, Attachments) ->
    Boundary = smtp_util:generate_message_boundary(),
    {"multipart/mixed; boundary=\""++Boundary++"\"",
        render_multipart_view([{"text/plain", Body}|Attachments], Boundary)}.

render_multipart_view(Parts, Boundary) ->
    ["This is a message with multiple parts in MIME format.\r\n",
        render_multipart_view1(Parts, Boundary)].

render_multipart_view1([], Boundary) ->
    ["--", Boundary, "--"];
render_multipart_view1([{FileName, MimeType, Body}|Rest], Boundary) ->
    ["--", Boundary, 
        "\r\n", "Content-Type: ", MimeType, 
        "\r\n", "Content-Disposition: attachment; filename=", FileName, 
        "\r\n", "Content-Transfer-Encoding: base64",
        "\r\n\r\n",
        wrap_to_76(base64:encode(erlang:iolist_to_binary(Body))), "\r\n", render_multipart_view1(Rest, Boundary)];
render_multipart_view1([{MimeType, Body}|Rest], Boundary) ->
    ["--", Boundary, "\r\n", "Content-Type: ", MimeType, "\r\n\r\n",
        Body, "\r\n", render_multipart_view1(Rest, Boundary)].

wrap_to_76(String) ->
    [wrap_to_76(String, [])].

wrap_to_76(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
wrap_to_76(<<Head:76/binary, Tail/binary>>, Acc) ->
    wrap_to_76(Tail, [<<"\r\n">>, Head | Acc]);
wrap_to_76(Head, Acc) ->
    list_to_binary(lists:reverse([<<"\r\n">>, Head | Acc])).
