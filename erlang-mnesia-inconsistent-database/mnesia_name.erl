-module(mnesia_name).

-compile(export_all).

change(From, To, Source, Target) ->
    Switch = fun(Node) when Node == From -> To;
                (Node) when Node == To -> throw({error, already_exists});
                (Node) -> Node
             end,
    Convert = fun({schema, db_nodes, Nodes}, Acc) ->
                      {[{schema, db_nodes, lists:map(Switch,Nodes)}], Acc};
                 ({schema, version, Version}, Acc) ->
                      {[{schema, version, Version}], Acc};
                 ({schema, cookie, Cookie}, Acc) ->
                      {[{schema, cookie, Cookie}], Acc};
                 ({schema, Tab, CreateList}, Acc) ->
                      Keys = [ram_copies, disc_copies, disc_only_copies],
                      OptSwitch =
                      fun({Key, Val}) ->
                              case lists:member(Key, Keys) of
                                  true -> {Key, lists:map(Switch, Val)};
                                  false-> {Key, Val}
                              end
                      end,
                      {[{schema, Tab, lists:map(OptSwitch, CreateList)}], Acc};
                 (Other, Acc) ->
                      {[Other], Acc}
              end,
    mnesia:traverse_backup(Source, Target, Convert, switched).

view(Source) ->
    View = fun(Item, Acc) ->
                   io:format("~p.~n",[Item]),
                   {[Item], Acc + 1}
           end,
    mnesia:traverse_backup(Source, dummy, read_only, View, 0).
