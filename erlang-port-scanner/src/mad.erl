-module(mad).

-export([scan/2]).

%% Internal exports.
-export([scan_addr_port/3]).

%% Test-only exports.
-export([generate_subnet_addrs/2,
         parse_addr_or_subnet/1]).

-export_type([s_ip4_address/0,
              s_ip4_subnet/0]).

-define(DEFAULT_TIMEOUT, 1000). %% milliseconds

-define(l2i(L), list_to_integer(L)).
-define(i2b(I), integer_to_binary(I)).
-define(b2i(B), binary_to_integer(I)).

%% IPv4 address: "1.2.3.4"
-type s_ip4_address() :: string().

%% IPv4 subnet: "1.2.3.0/24"
-type s_ip4_subnet() :: string().

%%
%% API
%%

-spec scan(AddrOrSubnet, Port) -> OpenPortsAddrs
      when AddrOrSubnet :: s_ip4_address() | s_ip4_subnet(),
           Port :: inet:port_number(),
           OpenPortsAddrs :: [{inet:ip4_address(), inet:port_number()}].
scan(AddrOrSubnet, Port) when is_integer(Port) ->
    scan(AddrOrSubnet, [Port]);
scan(AddrOrSubnet, Ports) ->
    Addrs = parse_addr_or_subnet(AddrOrSubnet),
    %% TODO: allow to customize timeout
    ScanOpts = [],
    scan_addrs_ports(Addrs, Ports, ScanOpts),
    receive_all(length(Addrs) * length(Ports), ScanOpts).

%%
%% Helpers
%%

-spec parse_addr_or_subnet(AddrOrSubnet) -> Addrs
      when AddrOrSubnet :: s_ip4_address() | s_ip4_subnet(),
           Addrs :: list(inet:ip4_address()).
parse_addr_or_subnet(AddrOrSubnet) ->
    case string:tokens(AddrOrSubnet, "/") of
        [Subnet, Mask] ->
            {ok, Addr} = inet:parse_ipv4_address(Subnet),
            generate_subnet_addrs(Addr, ?l2i(Mask));
        [SAddr] ->
            {ok, Addr} = inet:parse_ipv4_address(SAddr),
            [Addr]
    end.

generate_subnet_addrs(Subnet, Mask) ->
    %% {192, 168, 1, 1} = Subnet.
    {A, B, C, D} = Subnet,
    %% An IPv4 is essentially a 32-bit integer.
    <<Addr:32>> = <<A, B, C, D>>,
    %% Mask == 25 => Rest == 32 - 25 == 7
    Rest = 32 - Mask,
    <<Base:Mask, IterFrom:Rest>> = <<Addr:32>>,
    %% Iterate from IterFrom to 2^Rest.
    [ binary_to_ip4_address(<<Base:Mask, I:Rest>>)
      || I <- lists:seq(IterFrom, 1 bsl Rest - 1) ].

binary_to_ip4_address(Binary) ->
    <<A:8, B:8, C:8, D:8>> = Binary,
    {A, B, C, D}.

scan_addrs_ports(Addrs, Ports, Opts) ->
    [ erlang:spawn_monitor(?MODULE, scan_addr_port, [Addr, Port, Opts])
      || Addr <- Addrs, Port <- Ports ],
    verbose(Opts) andalso stderr("~n").

scan_addr_port(Addr, Port, Opts) ->
    verbose(Opts) andalso stderr("."),
    ConnectOpts = [inet, {packet, 0}],
    case gen_tcp:connect(Addr, Port, ConnectOpts, timeout(Opts)) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            exit({ok, Addr, Port});
        Error ->
            exit(Error)
    end.

timeout(Opts) -> get_opt(timeout, Opts, ?DEFAULT_TIMEOUT).

verbose(Opts) -> get_opt(verbose, Opts, false).

get_opt(Opt, Opts, Default) ->
    case lists:keyfind(Opt, 1, Opts) of
        {Opt, Val} -> Val;
        false -> Default
    end.

receive_all(N, Opts) ->
    receive_all(N, [], Opts).

receive_all(0, Acc, _) -> Acc;
receive_all(N, Acc, Opts) ->
    receive
        {'DOWN', _, process, _, {ok, Addr, Port}} ->
            receive_all(N-1, [{Addr, Port} | Acc], Opts);
        _ ->
            receive_all(N-1, Acc, Opts)
    after timeout(Opts) ->
        verbose(Opts) andalso stderr("error: timeout~n"),
        Acc
    end.

stderr(Msg) -> io:format(standard_error, Msg, []).
