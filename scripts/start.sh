#!/bin/bash

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)" 
ROOT=$DIR/..

ERL=erl
NAME_NODE=bluegene@localhost
COOKIE='node-cookie'
EBINS="$ROOT/deps/*/ebin $ROOT/apps/*/ebin $ROOT/ebin"


exec $ERL -name $NAME_NODE \
    -setcookie $COOKIE \
    -pa $EBINS -boot start_sasl \
    -eval "application:load(mochiweb)" \
    -eval "game_ctrl:server_start()"
    
