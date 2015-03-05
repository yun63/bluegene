# bluegene
==========

### 关于依赖

bluegene项目里deps目录下的所有子项目都来自于github,参见rebar.config文件中*deps*配置项.
为了使项目工程代码清晰,deps下的子项目都做了清理工作,只保留代码目录.

### 项目结构
```
.
├── apps
│   ├── gateway
│   │   ├── doc
│   │   │   ├── edoc-info
│   │   │   ├── erlang.png
│   │   │   ├── gateway_app.html
│   │   │   ├── gateway.html
│   │   │   ├── gateway_sup.html
│   │   │   ├── index.html
│   │   │   ├── modules-frame.html
│   │   │   ├── overview-summary.html
│   │   │   ├── packages-frame.html
│   │   │   └── stylesheet.css
│   │   ├── ebin
│   │   └── src
│   │       ├── gateway_app.erl
│   │       ├── gateway.app.src
│   │       ├── gateway.erl
│   │       └── gateway_sup.erl
│   ├── player_app
│   │   ├── doc
│   │   │   ├── edoc-info
│   │   │   ├── erlang.png
│   │   │   ├── index.html
│   │   │   ├── modules-frame.html
│   │   │   ├── overview-summary.html
│   │   │   ├── packages-frame.html
│   │   │   ├── player_app.html
│   │   │   ├── player_server.html
│   │   │   ├── player_sup.html
│   │   │   └── stylesheet.css
│   │   ├── ebin
│   │   └── src
│   │       ├── player_app.erl
│   │       ├── player.app.src
│   │       ├── player_server.erl
│   │       └── player_sup.erl
│   ├── scene
│   │   ├── doc
│   │   │   ├── area_app.html
│   │   │   ├── area_server.html
│   │   │   ├── area_sup.html
│   │   │   ├── edoc-info
│   │   │   ├── erlang.png
│   │   │   ├── index.html
│   │   │   ├── modules-frame.html
│   │   │   ├── overview-summary.html
│   │   │   ├── packages-frame.html
│   │   │   └── stylesheet.css
│   │   ├── ebin
│   │   └── src
│   │       ├── area_app.erl
│   │       ├── area.app.src
│   │       ├── area_server.erl
│   │       └── area_sup.erl
│   └── world
│       ├── doc
│       │   ├── edoc-info
│       │   ├── erlang.png
│       │   ├── index.html
│       │   ├── modules-frame.html
│       │   ├── overview-summary.html
│       │   ├── packages-frame.html
│       │   ├── stylesheet.css
│       │   ├── world_app.html
│       │   ├── world.html
│       │   └── world_sup.html
│       ├── ebin
│       └── src
│           ├── world_app.erl
│           ├── world.app.src
│           ├── world.erl
│           └── world_sup.erl
├── deps
│   ├── emysql
│   │   ├── doc
│   │   │   ├── edoc-info
│   │   │   ├── emysql_app.html
│   │   │   ├── emysql_conn_mgr.html
│   │   │   ├── emysql.html
│   │   │   ├── emysql_statements.html
│   │   │   ├── emysql_util.html
│   │   │   ├── emysql_worker.html
│   │   │   ├── erlang.png
│   │   │   ├── index.html
│   │   │   ├── modules-frame.html
│   │   │   ├── overview-summary.html
│   │   │   ├── packages-frame.html
│   │   │   └── stylesheet.css
│   │   ├── ebin
│   │   ├── etc
│   │   │   ├── issue20.erl
│   │   │   ├── issue7.erl
│   │   │   └── markdown.lua
│   │   ├── include
│   │   │   ├── crypto_compat.hrl
│   │   │   └── emysql.hrl
│   │   └── src
│   │       ├── emysql_app.erl
│   │       ├── emysql.app.src
│   │       ├── emysql_auth.erl
│   │       ├── emysql_conn.erl
│   │       ├── emysql_conn_mgr.erl
│   │       ├── emysql_conv.erl
│   │       ├── emysql.erl
│   │       ├── emysql_internal.hrl
│   │       ├── emysql_statements.erl
│   │       ├── emysql_sup.erl
│   │       ├── emysql_tcp.erl
│   │       ├── emysql_util.erl
│   │       ├── emysql_worker.erl
│   │       └── Makefile
│   ├── mochiweb
│   │   ├── doc
│   │   │   ├── edoc-info
│   │   │   ├── erlang.png
│   │   │   ├── index.html
│   │   │   ├── mochifmt.html
│   │   │   ├── mochifmt_records.html
│   │   │   ├── mochifmt_std.html
│   │   │   ├── mochiglobal.html
│   │   │   ├── mochihex.html
│   │   │   ├── mochijson2.html
│   │   │   ├── mochijson.html
│   │   │   ├── mochilists.html
│   │   │   ├── mochilogfile2.html
│   │   │   ├── mochinum.html
│   │   │   ├── mochitemp.html
│   │   │   ├── mochiutf8.html
│   │   │   ├── mochiweb_acceptor.html
│   │   │   ├── mochiweb_base64url.html
│   │   │   ├── mochiweb_charref.html
│   │   │   ├── mochiweb_cookies.html
│   │   │   ├── mochiweb_cover.html
│   │   │   ├── mochiweb_echo.html
│   │   │   ├── mochiweb_headers.html
│   │   │   ├── mochiweb.html
│   │   │   ├── mochiweb_html.html
│   │   │   ├── mochiweb_http.html
│   │   │   ├── mochiweb_io.html
│   │   │   ├── mochiweb_mime.html
│   │   │   ├── mochiweb_multipart.html
│   │   │   ├── mochiweb_request.html
│   │   │   ├── mochiweb_response.html
│   │   │   ├── mochiweb_session.html
│   │   │   ├── mochiweb_socket.html
│   │   │   ├── mochiweb_socket_server.html
│   │   │   ├── mochiweb_util.html
│   │   │   ├── mochiweb_websocket.html
│   │   │   ├── modules-frame.html
│   │   │   ├── overview-summary.html
│   │   │   ├── packages-frame.html
│   │   │   ├── reloader.html
│   │   │   └── stylesheet.css
│   │   ├── ebin
│   │   ├── include
│   │   │   └── internal.hrl
│   │   └── src
│   │       ├── mochifmt.erl
│   │       ├── mochifmt_records.erl
│   │       ├── mochifmt_std.erl
│   │       ├── mochiglobal.erl
│   │       ├── mochihex.erl
│   │       ├── mochijson2.erl
│   │       ├── mochijson.erl
│   │       ├── mochilists.erl
│   │       ├── mochilogfile2.erl
│   │       ├── mochinum.erl
│   │       ├── mochitemp.erl
│   │       ├── mochiutf8.erl
│   │       ├── mochiweb_acceptor.erl
│   │       ├── mochiweb.app.src
│   │       ├── mochiweb_base64url.erl
│   │       ├── mochiweb_charref.erl
│   │       ├── mochiweb_cookies.erl
│   │       ├── mochiweb_cover.erl
│   │       ├── mochiweb_echo.erl
│   │       ├── mochiweb.erl
│   │       ├── mochiweb_headers.erl
│   │       ├── mochiweb_html.erl
│   │       ├── mochiweb_http.erl
│   │       ├── mochiweb_io.erl
│   │       ├── mochiweb_mime.erl
│   │       ├── mochiweb_multipart.erl
│   │       ├── mochiweb_request.erl
│   │       ├── mochiweb_response.erl
│   │       ├── mochiweb_session.erl
│   │       ├── mochiweb_socket.erl
│   │       ├── mochiweb_socket_server.erl
│   │       ├── mochiweb_util.erl
│   │       ├── mochiweb_websocket.erl
│   │       └── reloader.erl
│   └── protobuffs
│       ├── bin
│       │   └── protoc-erl
│       ├── ebin
│       ├── src
│       │   ├── overview.edoc
│       │   ├── pokemon_pb.erl
│       │   ├── protobuffs.app.src
│       │   ├── protobuffs_compile.erl
│       │   ├── protobuffs.erl
│       │   ├── protobuffs_file.erl
│       │   ├── protobuffs_parser.yrl
│       │   └── protobuffs_scanner.xrl
│       └── test
│           ├── erlang_protobuffs_SUITE_data
│           │   ├── extend_in_reserved_range.proto
│           │   ├── extend_out_of_range.proto
│           │   └── proto
│           │       ├── addressbook.proto
│           │       ├── camel_case.proto
│           │       ├── delimed.proto
│           │       ├── empty.proto
│           │       ├── enum_outside.proto
│           │       ├── enum.proto
│           │       ├── exports.proto
│           │       ├── extend.proto
│           │       ├── extensions.proto
│           │       ├── hasdefault.proto
│           │       ├── import
│           │       │   └── import_target.proto
│           │       ├── import.proto
│           │       ├── imports.proto
│           │       ├── nested1.proto
│           │       ├── nested2.proto
│           │       ├── nested3.proto
│           │       ├── nested4.proto
│           │       ├── nested5.proto
│           │       ├── packed_repeated.proto
│           │       ├── repeater.proto
│           │       ├── service.proto
│           │       ├── simple.proto
│           │       ├── single.proto
│           │       └── special_words.proto
│           ├── erlang_protobuffs_SUITE.erl
│           ├── protobuffs_compile_tests.erl
│           ├── protobuffs_parser_tests.erl
│           ├── protobuffs_proper.erl
│           ├── protobuffs_tests.erl
│           └── quickcheck_setup.hrl
├── doc
├── Makefile
├── README.md
├── rebar
├── rebar.config
├── rel
└── scripts
```

