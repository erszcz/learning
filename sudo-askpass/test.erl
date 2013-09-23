-module(test).

-compile([export_all]).

virtual_iface_up() ->
    Cmd = filename:absname("") ++ "/../../tools/virtual-iface up",
    os:cmd(Cmd),
    timer:sleep(?DELAY).

virtual_iface_down() ->
    Cmd = filename:absname("") ++ "/../../tools/virtual-iface down",
    os:cmd(Cmd),
    timer:sleep(?DELAY).
