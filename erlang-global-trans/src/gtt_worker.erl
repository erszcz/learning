-module(gtt_worker).

-behaviour(gen_server).

%% API
-export([
         start_link/0
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(CLEANUP_NODEDOWN_LOCK, cleanup_nodedown_lock).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    net_kernel:monitor_nodes(true),
    io:format("[~p] monitoring nodes~n", [node()]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nodedown, Node}, State) ->
    cleanup_nodedown(Node),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

cleanup_nodedown(Node) ->
    C = fun () ->
                %do_cleanup_nodedown(Node)
                fail_when_cleanup_nodedown(Node)
        end,
    LockRequest = {?CLEANUP_NODEDOWN_LOCK, self()},
    Nodes = [node() | nodes()],
    SingleAttempt = 1,
    io:format("[~p] ~p went down, trying to get the lock...~n", [node(), Node]),
    case global:trans(LockRequest, C, Nodes, SingleAttempt) of
        aborted -> io:format("[~p] could not get the lock~n" , [node()]);
        ok -> io:format("[~p] cleanup done~n", [node()])
    end.

do_cleanup_nodedown(Node) ->
    io:format("[~p] cleaning up after ~p~n", [node(), Node]),
    timer:sleep(timer:seconds(1)),
    ok.

fail_when_cleanup_nodedown(Node) ->
    io:format("[~p] deliberate failure!~n", [node()]),
    erlang:halt(1).
