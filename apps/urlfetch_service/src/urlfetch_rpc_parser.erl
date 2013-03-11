-module(urlfetch_rpc_parser).
-export([parse/1]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% Parser for a very simple binary protocol.
parse(<<CLen:8/unsigned, Command:CLen/binary,
        A1Len:8/unsigned, Arg1:A1Len/binary,
        A2Len:8/unsigned, Arg2:A2Len/binary,
        A3Len:8/unsigned, Arg3:A3Len/binary,
        Rest/binary>>) ->
    {request, Command, Arg1, Arg2, Arg3, Rest};
parse(<<CLen:8/unsigned, Command:CLen/binary,
        A1Len:8/unsigned, Arg1:A1Len/binary,
        A2Len:8/unsigned, Arg2:A2Len/binary,
        Rest/binary>>) ->
    {request, Command, Arg1, Arg2, Rest};
parse(<<CLen:8/unsigned, Command:CLen/binary,
        ALen:8/unsigned, Arg:ALen/binary, Rest/binary>>) ->
    {request, Command, Arg, Rest};
parse(<<CLen:8/unsigned, Command:CLen/binary, Rest/binary>>) ->
    {request, Command, Rest};
parse(Bin) ->
    {noreply, Bin}.

-ifdef(TEST).
%% Unit tests.
parse_test() ->
    %% Correct protocol
    ?assert(parse(<<4, "TEST", 3, "foo", 3, "bar", 21,
                    "A much longer string.", "test">>) =:=
            {request, <<"TEST">>, <<"foo">>, <<"bar">>,
                      <<"A much longer string.">>,
                      <<"test">>}),
    ?assert(parse(<<4, "TEST", 3, "foo", 3, "bar", "test">>) =:=
            {request, <<"TEST">>, <<"foo">>, <<"bar">>, <<"test">>}),
    ?assert(parse(<<4, "TEST", 3, "foo", "bar">>) =:=
            {request, <<"TEST">>, <<"foo">>, <<"bar">>}),
    ?assert(parse(<<4, "TEST", "foo">>) =:= {request, <<"TEST">>, <<"foo">>}),
    %% Bogus data
    ?assert(parse(<<"foobar">>) =:= {noreply, <<"foobar">>}).
-endif.
