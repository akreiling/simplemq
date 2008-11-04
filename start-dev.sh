#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin -s simplemq
