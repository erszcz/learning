-module(driver).
-compile([export_all]).

%%
%% Constants
%%

actions() ->
    [#{op => stop, trap_exit => true},
     #{op => crash, trap_exit => true},
     #{op => external_normal_exit, trap_exit => true},
     #{op => external_abnormal_exit, trap_exit => true},
     #{op => external_shutdown, trap_exit => true},
     %% TODO: op => parent_shutdown / supervisor_shutdown
     #{op => linked_process_exits, trap_exit => true},
     #{op => linked_process_crashes, trap_exit => true},

     #{op => stop, trap_exit => false},
     #{op => crash, trap_exit => false},
     #{op => external_normal_exit, trap_exit => false},
     #{op => external_abnormal_exit, trap_exit => false},
     #{op => external_shutdown, trap_exit => false},
     #{op => linked_process_exits, trap_exit => false},
     #{op => linked_process_crashes, trap_exit => false}].

%%
%% API
%%

test() ->
    Rs = [ do(A) || A <- actions() ],
    [ print("~3b) ~s~n", [I, format_result(R)]) || {I, R} <- enum(Rs) ].

do(#{op := Op, trap_exit := Trap}) ->
    try
        CRef = make_ref(),
        {ok, Pid} = server:start(self(), CRef),
        MRef = erlang:monitor(process, Pid),
        if
            Trap -> server:trap_exit(Pid, Trap);
            not Trap -> ok
        end,
        catch server:Op(Pid),
        TStatus = receive
                      {terminated, CRef} -> called
                  after timer:seconds(1) -> not_called
                  end,
        MStatus = receive
                      {'DOWN', MRef, _, _, _} ->
                          ok
                  after timer:seconds(6) ->
                          timeout
                  end,
        #{op => Op,
          trap_exit => Trap,
          terminate_status => TStatus,
          monitor_status => MStatus,
          aliveness => erlang:is_process_alive(Pid)}
    catch
        _:R -> print("driver error: ~p~n", [R]),
               driver_error
    end.

%%
%% Internal
%%

print(Fmt, Args) ->
    io:format(Fmt, Args).

format_result(driver_error) -> "driver_error";
format_result(#{op := Op, trap_exit := Trap,
                terminate_status := TStatus,
                aliveness := Alive}) ->
    io_lib:format("trap:~6s\top:~23s\talive:~6s\tterminate:~11s",
                  [Trap, Op, Alive, TStatus]).

enum(List) ->
    lists:zip(lists:seq(1, length(List)), List).
