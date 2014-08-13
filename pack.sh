#!/bin/sh
VERSION=0.1
NAME=eshell-git
DIR=$NAME-$VERSION
TAR=$NAME-$VERSION.tar
mkdir -p $DIR
cp *.el README $DIR
tar -c -f $TAR $DIR
rm -rf $DIR
export RELEASE_FILE=$TAR
