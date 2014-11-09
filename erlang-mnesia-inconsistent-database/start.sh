#!/usr/bin/env bash

NODE=$1
mkdir -p Mnesia.${NODE}
erl -sname ${NODE} -setcookie cookie -mnesia dir \"Mnesia.${NODE}\" -s mnesia
