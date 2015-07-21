-module(mlp).
-compile([export_all]).

main([]) ->
    loop(standard_io).

loop(Handle) ->
    case file:read_line(Handle) of
        {ok, Data} ->
            process_line(Data),
            loop(Handle);
        eof -> ok;
        {error, Reason} -> io:format(standard_error, "error: ~p", [Reason])
    end.

process_line(Data) -> dispatch(Data).

dispatch(Data) ->
    Actions = [ {"mod_bosh:info:208 Sending (binary) to", fun parse_bosh_sending/2},
                {"mod_bosh:info:204 Parsed body:", fun parse_bosh_parsed/2} ],
    {Pattern, Action} = lists:foldl(pa:bind(fun match_line/3, Data),
                                    {"(no match)", fun no_action/2}, Actions),
    Action(Pattern, Data).

%% 2015-04-02 09:32:33.110 [debug] <0.15387.42>@mod_bosh:info:208 Sending (binary) to
%%  <<"8f171e2905fa08c9c2c0cea6d723f3cd99810680">>: <<"<body maxpause='120' inactivity='30'
%%  xmlns:stream='http://etherx.jabber.org/streams' xmlns:xmpp='urn:xmpp:xbosh' 
%%  xmlns='http://jabber.org/protocol/httpbind' xmpp:version='1.0' xmpp:restartlogic='true' 
%%  sid='8f171e2905fa08c9c2c0cea6d723f3cd99810680' accept='deflate,gzip' from='excellencemotors.com' 
%%  hold='1' requests='2' wait='60'><stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/>
%%  <session xmlns='urn:ietf:params:xml:ns:xmpp-session'/><sm xmlns='urn:xmpp:sm:3'/></stream:features></body>">>
parse_bosh_sending(Pattern, Data) ->
    LastIndex = string:rstr(Data, Pattern),
    PastPattern = LastIndex + length(Pattern),
    PacketIndex = PastPattern + string:str(string:substr(Data, PastPattern), ">>: ") + length(">>: ") - 1,
    PacketAsStringifiedBinary = string:substr(Data, PacketIndex),
    BPacket = eval(PacketAsStringifiedBinary ++ ".", []),
    case exml:parse(BPacket) of
        {ok, Element} ->
            io:format("<< bosh sent:~n~n~s~n", [exml:to_pretty_iolist(Element)]);
        _ ->
            io:format("<< bosh sent (unparseable):~n~n~p~n~n", [BPacket])
    end.

%% 2015-04-02 09:32:33.300 [debug] <0.15387.42>@mod_bosh:info:204 Parsed body: 
%%  {xmlel,<<"body">>,[{<<"xmlns">>,<<"http://jabber.org/protocol/httpbind">>},
%% {<<"rid">>,<<"2796558209">>},{<<"sid">>,<<"8f171e2905fa08c9c2c0cea6d723f3cd99810680">>}],
%% [{xmlel,<<"iq">>,[{<<"xmlns">>,<<"jabber:client">>},{<<"type">>,<<"set">>},{<<"id">>,<<"_bind_auth_2">>}],
%% [{xmlel,<<"bind">>,[{<<"xmlns">>,<<"urn:ietf:params:xml:ns:xmpp-bind">>}],
%% [{xmlel,<<"resource">>,[],[{xmlcdata,<<"FOEdge_i80ds0ls-1s3d">>}]}]}]}]}
parse_bosh_parsed(Pattern, Data) ->
    PacketIndex = string:rstr(Data, Pattern) + length(Pattern) + 1,
    StringifiedPacketTerm = string:substr(Data, PacketIndex),
    Element = eval(StringifiedPacketTerm ++ ".", []),
    io:format(">> bosh received:~n~n~s~n", [exml:to_pretty_iolist(Element)]).

no_action(_Pattern, _Data) ->
    ok.
    %case string:strip(Data, both) of
    %    [] -> ok;
    %    %__ -> stderr("no action for: ~s~n", [string:substr(Data, 40) ++ "..."])
    %    __ -> stderr("no action for: ~p~n", [Data])
    %end.

match_line(Data, {Pattern, Action}, Acc) ->
    case 'contains?'(Data, Pattern) of
        true -> {Pattern, Action};
        false -> Acc
    end.

'contains?'(String, Pattern) ->
    string:str(String, Pattern) /= 0.

stderr(Format, Args) ->
    io:format(standard_io, Format, Args).

eval(S, Environ) ->
    {ok, Scanned, _} = erl_scan:string(S),
    {ok, Parsed} = erl_parse:parse_exprs(Scanned),
    {value, Term, []} = erl_eval:exprs(Parsed, Environ),
    Term.
