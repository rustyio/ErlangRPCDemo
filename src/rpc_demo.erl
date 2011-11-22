-module(rpc_demo).
-export([
         test/0,
         json_call/1,
         bert_call/5,
         pb_call/2, pb_call/3
        ]).
-include_lib("eunit/include/eunit.hrl").
-include("rpc_demo_pb.hrl").

test() ->
    eunit:test(rpc_demo).

webmachine_test() ->
    Expected = sequence:sequence(5),
    Actual = json_call("http://localhost:8001/sequence/5"),
    ?assertEqual(Expected, Actual).

spooky_test() ->
    Expected = sequence:sequence(5),
    Actual = json_call("http://localhost:8002/sequence/5"),
    ?assertEqual(Expected, Actual).

bert_test() ->
    Expected = sequence:sequence(5),
    Actual = bert_call("localhost", 9999, ernie_sequence, sequence, [5]),
    ?assertEqual(Expected, Actual).

pb_test() ->
    Expected = sequence:sequence(5),
    {ok, Response} = pb_call("localhost", 8003, #sequencerequest { n = 5 }),
    Actual = {ok, Response#sequenceresponse.sequence},
    ?assertEqual(Expected, Actual).

%%% Helper Functions %%%

json_call(Url) ->
    Headers = [{"Content-Type", "application/json"}],
    Request = {Url, Headers},
    case httpc:request(get, Request, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, mochijson2:decode(Body)};
        Other ->
            {error, Other}
    end.

bert_call(Host, Port, Mod, Fun, Args) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 4}]) of
        {ok, Sock} ->
            bert_call_1(Sock, Mod, Fun, Args);
        Other ->
            io:format("Unable to establish connection: ~p~n", [Other])
    end.
bert_call_1(Sock, Mod, Fun, Args) ->
    RequestBytes = bert:encode({call, Mod, Fun, Args}),
    ok = gen_tcp:send(Sock, RequestBytes),
    receive
        {tcp, _Port, ResponseBytes} ->
            ok = gen_tcp:close(Sock),
            case bert:decode(ResponseBytes) of
                {reply, Reply} ->
                    Reply;
                Other ->
                    {error, {unexpected_response, Other}}
            end;
        {tcp_closed, Port} ->
            io:format("Connection closed after sending data: ~p~n", [Port]),
            {error, {tcp_closed, Port}};
        Other ->
            ok = gen_tcp:close(Sock),
            io:format("Unexpected message: ~p~n", [Other]),
            {error, Other}
    end.


pb_call(Host, Port, Req) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 4}, {header, 1}]) of
        {ok, Sock} ->
            pb_call(Sock, Req);
        Other ->
            io:format("Unable to establish connection: ~p~n", [Other])
    end.
pb_call(Sock, Req) ->
    RequestBytes = protobuff_server:encode(Req),
    ok = gen_tcp:send(Sock, RequestBytes),
    receive
        {tcp, _Port, ResponseBytes} ->
            ok = gen_tcp:close(Sock),
            {ok, protobuff_server:decode(ResponseBytes)};
        {tcp_closed, Port} ->
            io:format("Connection closed after sending data: ~p~n", [Port]),
            {error, {tcp_closed, Port}};
        Other ->
            ok = gen_tcp:close(Sock),
            io:format("Unexpected message: ~p~n", [Other]),
            {error, Other}
    end.
