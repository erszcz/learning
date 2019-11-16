#!/usr/bin/env python
#
# AccessDump.py
# A simple script to dump the contents of a Microsoft Access Database.
# It depends upon the mdbtools suite:
#   http://sourceforge.net/projects/mdbtools/

import sys, subprocess, os

DATABASE = sys.argv[1]

# Dump the schema for the DB
subprocess.call(["mdb-schema", DATABASE, "mysql"])

# Get the list of table names with "mdb-tables"
table_names = subprocess.Popen(["mdb-tables", "-1", DATABASE],
                               stdout=subprocess.PIPE).communicate()[0]
tables = table_names.splitlines()

print "BEGIN;" # start a transaction, speeds things up when importing
sys.stdout.flush()

# Dump each table as a CSV file using "mdb-export",
# converting " " in table names to "_" for the CSV filenames.
for table in tables:
    if table != '':
        subprocess.call(["mdb-export", "-I", "mysql", DATABASE, table])

print "COMMIT;" # end the transaction
sys.stdout.flush()