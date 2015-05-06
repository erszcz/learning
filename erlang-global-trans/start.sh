#!/usr/bin/env bash

NODENAME="$1"
COOKIE="somecookie"

exec erl -sname ${NODENAME} -setcookie ${COOKIE} -pa ebin
