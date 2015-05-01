-module(epmd_propfile).
-behaviour(gen_server).

%% API
-export([start_link/0,
         stop/0,
         port_please/2, port_please/3,
         names/0, names/1,
         register_node/2

         %% These erl_epmd exports are not known to be used.
         %% An xref analysis did not show any call sites in the whole of OTP.
         %% open/0, open/1, open/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(EPMD_PROPFILE, "/tmp/epmd.dat").
-define(SERVER, erl_epmd).
%% This probably should not be hardcoded, but I don't know yet what it means.
-define(VERSION, 5).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop, infinity).

%% (<0.53.0>) call erl_epmd:port_please("nktest",{127,0,1,1})
%% (<0.53.0>) returned from erl_epmd:port_please/2 -> {port,41509,5}
port_please(Node, Host) ->
    port_please(Node, Host, infinity).

port_please(Node, _EpmdAddr, _Timeout) ->
    get_port(Node, ?EPMD_PROPFILE).

%% (<0.50.0>) call erl_epmd:register_node(nktest,41509)
%% (<0.50.0>) returned from erl_epmd:register_node/2 -> {ok,3}
register_node(Name, PortNo) ->
    do_register_node(?EPMD_PROPFILE, Name, PortNo).

names() -> 
    {ok, H} = inet:gethostname(),
    names(H).

%% (<0.70.0>) call erl_epmd:names({127,0,1,1})
%% (<0.70.0>) returned from erl_epmd:names/1 -> {ok,[{"epmd-hacks",46293}]}
names(_EpmdAddr) ->
    get_names(?EPMD_PROPFILE).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({register, Name, PortNo} = Request, _From, State) ->
    io:format("request: ~p~n", [Request]),
    try register_node(Name, PortNo) of
        {ok, Creation} ->
            {reply, {ok, Creation}, State}
    catch
        _:Reason ->
            {reply, {error, Reason}, State}
    end;

handle_call(stop, _From, State) ->
    {stop, shutdown, ok, State};

handle_call(Request, _From, State) ->
    io:format("request: ~p~n", [Request]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    io:format("msg: ~p~n", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    io:format("info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_register_node(EpmdPropfile, Name, PortNo) ->
    RegisteredNodes = case file:consult(EpmdPropfile) of
                          {ok, [Nodes]} -> Nodes;
                          _ -> []
                      end,
    erlang:display(RegisteredNodes),
    FullName = full_name(Name),
    {NewNodes,
     Creation} = case lists:keyfind(FullName, 2, RegisteredNodes) of
                     false ->
                         { [{node, FullName, PortNo, 1} | RegisteredNodes], 1 };
                     {node, FullName, _OldPort, OldCreation} ->
                         C = OldCreation + 1,
                         { lists:keystore(FullName, 2, RegisteredNodes,
                                          {node, FullName, PortNo, C}),
                           C }
                 end,
    ok = file:write_file(EpmdPropfile,
                         io_lib:format("~p.~n", [lists:sort(NewNodes)])),
    {ok, Creation}.

full_name(Name) ->
    {ok, HostName} = inet:gethostname(),
    if
        is_atom(Name) -> atom_to_list(Name);
        is_list(Name) -> Name
    end ++ "@" ++ HostName.

name_and_domain(Node) ->
    [N, D] = string:tokens(Node, "@"),
    {N, D}.

-spec get_port(Node, EpmdPropfile) -> PortSpec | noport when
      Node :: atom() | list(),
      EpmdPropfile :: list(),
      PortSpec :: {port, Port, Version},
      Port :: non_neg_integer(),
      Version :: 5.
get_port(Node, EpmdPropfile) ->
    FullName = full_name(Node),
    case file:consult(EpmdPropfile) of
        {error, _Reason} ->
            %error(Reason, [Node, EpmdPropfile])
            noport;
        {ok, [Nodes]} ->
            get_port_from_nodes(FullName, Nodes)
    end.

get_port_from_nodes(FullName, Nodes) ->
    case lists:keyfind(FullName, 2, Nodes) of
        false -> noport;
        {node, FullName, Port, _Creation} -> {port, Port, ?VERSION}
    end.

%% (<0.70.0>) call erl_epmd:names({127,0,1,1})
%% (<0.70.0>) returned from erl_epmd:names/1 -> {ok,[{"epmd-hacks",46293}]}
get_names(EpmdPropfile) ->
    {ok, TargetHostName} = inet:gethostname(),
    get_names(EpmdPropfile, TargetHostName).

get_names(EpmdPropfile, TargetHostName) ->
    case file:consult(EpmdPropfile) of
        {error, _} ->
            {ok, []};
        {ok, [Nodes]} ->
            {ok, [ {Name, Port} || {node, Node, Port, _} <- Nodes,
                                   {Name, Host} <- [name_and_domain(Node)],
                                   Host == TargetHostName ]}
    end.
