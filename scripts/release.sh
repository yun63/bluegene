#!/bin/bash

ROOT=../
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

APPS_DIR=$DIR/../apps
DEPS_DIR=$DIR/../deps
REL_DIR=$DIR/../rel

if [ -d $REL_DIR ]; then
    rm -rf $REL_DIR
fi

mkdir -p $REL_DIR/ebin $REL_DIR/etc

for EBIN in `find $APPS_DIR $DEPS_DIR -name "ebin" -type d`; do
    cp -rf $EBIN/* $REL_DIR/ebin
done

cp -rf $ROOT/etc $REL_DIR/etc




