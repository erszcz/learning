-module(h).

-compile([export_all]).

save(File) ->
    proc_lib:spawn(?MODULE, do_save, [File]).

do_save(File) ->
    T = ets:new(shell_group_hist, []),
    dets:to_ets(shell_group_hist, T),
    Lines = [L || {_,L} <- lists:sort(ets:tab2list(T))],
    ok = file:write_file(File, unicode:characters_to_binary(Lines)).
