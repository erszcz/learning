# Static DNS name resolution

```
Lookup = [file | lists:delete(file, proplists:get_value(lookup, inet_db:get_rc()))].
inet_db:set_lookup(Lookup).
inet_db:add_rc_list([{host, {127,0,0,1}, ["myhostname"]}]).
inet:getaddr("myhostname", inet).
```

Result:

```
{ok,{127,0,0,1}}
```
