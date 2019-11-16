#!/usr/bin/env sh
# prerequisite https://github.com/brianb/mdbtools
#
# if your mdb is encoded in certain character set
# do something like this
# export MDB_ICONV="Big-5"
# export MDB_JET3_CHARSET="Big-5"
#
# under zsh
# for f (**/*.mdb) {~/dump.sh $f}
#

python ./AccessDump.py $1 | sqlite3 $1.sqlite

for x in `mdb-tables -1 $1`; do mdb-export $1 $x >> $1.$x.csv ; done
