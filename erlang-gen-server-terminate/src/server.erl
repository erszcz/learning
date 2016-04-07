-module(server).

-behaviour(gen_server).

%% API
-export([start_link/2,
         start/2,
         trap_exit/2,
         stop/1,
         external_normal_exit/1,
         external_abnormal_exit/1,
         external_shutdown/1,
         crash/1,
         linked_process_exits/1,
         linked_process_crashes/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Debug
-export([trace/0, trace/1]).

%%
%% API
%%

start_link(Parent, Ref) ->
    gen_server:start_link(?MODULE, [Parent, Ref], []).

start(Parent, Ref) ->
    gen_server:start(?MODULE, [Parent, Ref], []).

trap_exit(Pid, Enabled) ->
    gen_server:call(Pid, {trap_exit, Enabled}).

stop(Pid) ->
    gen_server:call(Pid, stop).

crash(Pid) ->
    gen_server:call(Pid, crash).

external_normal_exit(Pid) ->
    exit(Pid, normal).

external_abnormal_exit(Pid) ->
    exit(Pid, abnormal).

external_shutdown(Pid) ->
    exit(Pid, shutdown).

linked_process_exits(Pid) ->
    gen_server:cast(Pid, linked_process_exits).

linked_process_crashes(Pid) ->
    gen_server:cast(Pid, linked_process_crashes).

%%
%% gen_server callbacks
%%

init([Parent, Ref]) ->
    {ok, #{parent => Parent, ref => Ref}}.

handle_call({trap_exit, Enabled}, _From, State) ->
    process_flag(trap_exit, Enabled),
    {reply, ok, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(crash, _From, State) ->
    _ = 1 / 0,
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(linked_process_exits, State) ->
    Pid = spawn_link(fun () -> ok end),
    MRef = erlang:monitor(process, Pid),
    receive {'DOWN', MRef, _, _, _} -> ok
    after 1000 -> ok
    end,
    {noreply, State};
handle_cast(linked_process_crashes, State) ->
    Pid = spawn_link(fun () -> _ = 1 / 0 end),
    MRef = erlang:monitor(process, Pid),
    receive {'DOWN', MRef, _, _, _} -> ok
    after 500 -> ok
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    maps:get(parent, State) ! {terminated, maps:get(ref, State)}, 
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Debug
%%

trace() ->
    trace(x).

trace(Flags) ->
    dbg:tpl(?MODULE, Flags).

%%
%% Internal functions
%%

print(Text) ->
    print(Text, []).

print(Fmt, Args) ->
    io:format(Fmt, Args).
