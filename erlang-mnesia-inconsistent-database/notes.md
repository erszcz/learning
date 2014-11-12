# Exercise: inconsistent database

Please run the following in distinct shell instances. Shell A:

```erlang
./start.sh a
rd(row, {id, data}).
application:start(mnesia).
mnesia:create_table(row, [{attributes, record_info(fields, row)}]).
mnesia:activity(transaction, fun mnesia:write/1, [#row{id=1, data="ala"}]).
mnesia:activity(transaction, fun mnesia:write/1, [#row{id=2, data="bolo"}]).
```

Shell B:

```erlang
./start.sh b
application:start(mnesia).
mnesia:change_config(extra_db_nodes, [a@x2]).
mnesia:add_table_copy(row, b@x2, ram_copies).
net_kernel:disconnect(a@x2).
net_kernel:connect(a@x2).
```

Right now we should see a Mnesia error: `mnesia_event got
{inconsistent_database, running_partitioned_network, a@x2}`.

# Mnesia running partitioned network

See [1] for a quick summary.

Refer to `mnesia/src/mnesia_monitor.erl` for the algorithm presented below.

Informally, partitioned network occurs when two nodes detect network
failure (`nodedown`) but continue operating.
Partitioned network event does not occur when one of the nodes cleanly
shuts down the Mnesia application.

Detecting a partitioned network consists of the following steps:

1. Receiving `{nodeup, Node}`.
2. Checking whether we previously registered `{nodedown, Node}`.
3. If yes, checking whether `Node` registered `nodedown` about us.
4. If yes, we are running partitioned network - notify the applications.

Specifically, there's nothing about comparing table versions in the above
protocol or about trying to reconcile table contents.

[1]: http://stackoverflow.com/questions/624570/online-mnesia-recovery-from-network-partition


# Table contents changing when one node goes down

When a Mnesia node is cleanly shutdown, a replicated table content changes
and then the node is brought up again, the table might be in two states:

- either it will be synced with the running replica (which
  replica is chosen and how?) if the `master_nodes` property of the table
  is not set (cf. `mnesia:table_info(Table, master_nodes)`) or set to one
  of the running replicas,

- or it will be read from disk (`disc_copies`) / empty (`ram_copies`) if
  the `master_nodes` property is set to the node in question.

**This might lead to inconsistency between table contents on different nodes!**
When a node is its own master for a given table, it won't properly
reconcile table contents with other nodes on Mnesia startup.

# Exercise: content changing when one node is down

Shell A:

```erlang
./start.sh a
rd(row, {id, data}).
application:start(mnesia).
mnesia:change_table_copy_type(schema, node(), disc_copies).
mnesia:create_table(row, [{attributes, record_info(fields, row)}]).
mnesia:activity(transaction, fun mnesia:write/1, [#row{id=1, data="ala"}]).
mnesia:activity(transaction, fun mnesia:write/1, [#row{id=2, data="bolo"}]).
ets:tab2list(row).
```

Shell B:

```erlang
./start.sh b
application:start(mnesia).
mnesia:change_config(extra_db_nodes, [a@x4]).
mnesia:change_table_copy_type(schema, node(), disc_copies).
mnesia:add_table_copy(row, b@x4, ram_copies).
ets:tab2list(row).
erlang:halt().
```

Shell A when node B is down:

```erlang
mnesia:activity(transaction, fun mnesia:delete/1, [{row, 1}]).
ets:tab2list(row).
```

Again shell B:

```erlang
./start.sh b
application:start(mnesia).
ets:tab2list(row).
```
