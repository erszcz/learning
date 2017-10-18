-module(server).

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
-define(PORT, 5678).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->

    dbg:tracer(),
    dbg:p(all, call),
    dbg:tpl(?MODULE, handle_info, x),
    dbg:tpl(dtls, listen, x),

    {ok, Socket} = gen_udp:open(?PORT, [{active, once}, binary]),
    {ok, #{udp_socket => Socket}}.

handle_call(Request, _From, State) ->
    Reply = ok,
    print("handle_call::request: ~p\n"
          "handle_call::reply  : ~p\n", [Request, Reply]),
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    print("handle_cast::msg: ~p\n", [Msg]),
    {noreply, State}.

handle_info({udp, Socket, IP, InPortNo, <<"dtls", Rest/bytes>>}, #{udp_socket := Socket}) ->
    print("handle_info::upgrade-to-dtls: ~p ~p ~p discarding: ~p\n",
          [Socket, IP, InPortNo, Rest]),
    %% Just to make sure...
    ok = inet:setopts(Socket, [{active, false}]),
    {ok, DTLSSocket} = dtls:listen(Socket, [{active, once}, binary]),
    {noreply, #{dtls_socket => DTLSSocket}};

handle_info({udp, Socket, IP, InPortNo, Data}, #{udp_socket := Socket} = State) ->
    print("handle_info::udp: ~p ~p ~p ~p\n", [Socket, IP, InPortNo, Data]),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info({udp, Socket, IP, InPortNo, Data}, #{dtls_socket := Socket} = State) ->
    print("handle_info::dtls: ~p ~p ~p ~p\n", [Socket, IP, InPortNo, Data]),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info(Info, State) ->
    print("handle_info::info: ~p\n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

print(Fmt, Args) ->
    io:format(Fmt, Args).
