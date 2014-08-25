# Idea

In this snippet I'm trying to answer the following question:
is it possible for a `gen_server` to accept a call
by returning `{noreply, NewState}` and then delegating the reply
to a worker process?
In other words, is it possible that a call reply comes from a different
process than `Server` in `gen_server:call(Server, Request)`?
