#!/usr/bin/env bash

prog=$1
${prog} > test.txt
diff example.txt test.txt > /dev/null || echo error
