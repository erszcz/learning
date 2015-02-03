-module(mad).

-export([scan/2]).

-compile(export_all).

-export_type([s_ip4_address/0,
              s_ip4_subnet/0]).

-define(l2i(L), list_to_integer(L)).
-define(i2b(I), integer_to_binary(I)).
-define(b2i(B), binary_to_integer(I)).

%% IPv4 address: "1.2.3.4"
-type s_ip4_address() :: string().

%% IPv4 subnet: "1.2.3.0/24"
-type s_ip4_subnet() :: string().

scan(AddrOrSubnet, Port) when is_integer(Port) ->
    scan(AddrOrSubnet, [Port]);
scan(AddrOrSubnet, Ports) ->
    Addrs = parse_addr_or_subnet(AddrOrSubnet),
    ok.

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
