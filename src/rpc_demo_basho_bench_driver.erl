%% Erlang RPC Demo
%% Copyright (c) 2011 Rusty Klophaus (@rustyio)
%% See MIT-LICENSE for licensing information.

-module(rpc_demo_basho_bench_driver).

-export([new/1,
         run/4]).


-include("rpc_demo_pb.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {
          webmachine_host,
          webmachine_port,
          spooky_host,
          spooky_port,
          bert_host,
          bert_port,
          pb_host,
          pb_port,
          pb_socket,
          expected,
          n
         }).


%% ====================================================================
%% API
%% ====================================================================

new(_Id) ->
    %% Ensure that inets is started...
    application:start(inets),
    inets:start(httpc, [{profile, keep_alive}]),
    Options1 = [
               {max_sessions, 10},
               {max_keep_alive_length, 10},
               {keep_alive_timeout, 120000}
              ],
    httpc:set_options(Options1, keep_alive),

    inets:start(httpc, [{profile, keep_dead}]),
    Options2 = [
               {max_sessions, 0},
               {max_keep_alive_length, 0},
               {keep_alive_timeout, 1}
              ],
    httpc:set_options(Options2, keep_dead),

    ensure_module(mochijson2),

    %% Read config settings...
    N = basho_bench_config:get(n, 5),
    State = #state {
      n = N,
      expected        =  sequence:sequence(N),
      webmachine_host = basho_bench_config:get(webmachine_host, "127.0.0.1"),
      webmachine_port = basho_bench_config:get(webmachine_port, 8001),
      spooky_host     = basho_bench_config:get(spooky_host, "127.0.0.1"),
      spooky_port     = basho_bench_config:get(spooky_port, 8002),
      bert_host       = basho_bench_config:get(bert_host, "127.0.0.1"),
      bert_port       = basho_bench_config:get(bert_port, 9999),
      pb_host         = basho_bench_config:get(pb_host, "127.0.0.1"),
      pb_port         = basho_bench_config:get(pb_port, 8003)},

    PBHost = State#state.pb_host,
    PBPort = State#state.pb_port,
    io:format("[~s:~p] DEBUG - PBHost: ~p~n", [?MODULE, ?LINE, PBHost]),
    io:format("[~s:~p] DEBUG - PBPort: ~p~n", [?MODULE, ?LINE, PBPort]),
    case gen_tcp:connect(PBHost, PBPort, [binary, {packet, 4}, {header, 1}]) of
        {ok, Socket} ->
            State1 = State#state { pb_socket = Socket},
            {ok, State1};
        Other ->
            io:format("[~s:~p] DEBUG - Other: ~p~n", [?MODULE, ?LINE, Other]),
            {error, Other}
    end.

run(webmachine_call, _KeyGen, _ValueGen, State) ->
    Host = State#state.webmachine_host,
    Port = State#state.webmachine_port,
    N = State#state.n,
    Expected = State#state.expected,
    URL = lists:flatten(io_lib:format("http://~s:~p/sequence/~p", 
                                      [Host, Port, N])),
    Actual = rpc_demo:json_call(URL, keep_dead),
    assertEqual(Expected, Actual),
    {ok, State};

run(webmachine_reuse_call, _KeyGen, _ValueGen, State) ->
    Host = State#state.webmachine_host,
    Port = State#state.webmachine_port,
    N = State#state.n,
    Expected = State#state.expected,
    URL = lists:flatten(io_lib:format("http://~s:~p/sequence/~p", 
                                      [Host, Port, N])),
    Actual = rpc_demo:json_call(URL, keep_alive),
    assertEqual(Expected, Actual),
    {ok, State};

run(spooky_call, _KeyGen, _ValueGen, State) ->
    Host = State#state.spooky_host,
    Port = State#state.spooky_port,
    N = State#state.n,
    Expected = State#state.expected,
    URL = lists:flatten(io_lib:format("http://~s:~p/sequence/~p", 
                                      [Host, Port, N])),
    Actual = rpc_demo:json_call(URL, keep_dead),
    assertEqual(Expected, Actual),
    {ok, State};

run(spooky_reuse_call, _KeyGen, _ValueGen, State) ->
    Host = State#state.spooky_host,
    Port = State#state.spooky_port,
    N = State#state.n,
    Expected = State#state.expected,
    URL = lists:flatten(io_lib:format("http://~s:~p/sequence/~p", 
                                      [Host, Port, N])),
    Actual = rpc_demo:json_call(URL, keep_alive),
    assertEqual(Expected, Actual),
    {ok, State};

run(bert_call, _KeyGen, _ValueGen, State) ->
    Host = State#state.bert_host,
    Port = State#state.bert_port,
    N = State#state.n,
    Expected = State#state.expected,
    Actual = rpc_demo:bert_call(Host, Port, ernie_sequence, sequence, [N]),
    assertEqual(Expected, Actual),
    {ok, State};

run(pb_call, _KeyGen, _ValueGen, State) ->
    Host = State#state.pb_host,
    Port = State#state.pb_port,
    N = State#state.n,
    Expected = State#state.expected,
    {ok, Response} = rpc_demo:pb_call(Host, Port, #sequencerequest { n = N }),
    Actual = {ok, Response#sequenceresponse.sequence},
    assertEqual(Expected, Actual),
    {ok, State};

run(pb_reuse_call, _KeyGen, _ValueGen, State) ->
    Socket = State#state.pb_socket,
    N = State#state.n,
    Expected = State#state.expected,
    {ok, Response} = rpc_demo:pb_call(Socket, #sequencerequest { n = N }),
    Actual = {ok, Response#sequenceresponse.sequence},
    assertEqual(Expected, Actual),
    {ok, State};

run(json_encode, _KeyGen, _ValueGen, State) ->
    {ok, Expected} = State#state.expected,
    Actual = mochijson2:decode(mochijson2:encode(Expected)),
    assertEqual(Expected, Actual),
    {ok, State};

run(bert_encode, _KeyGen, _ValueGen, State) ->
    {ok, Expected} = State#state.expected,
    Actual = bert:decode(bert:encode(Expected)),
    assertEqual(Expected, Actual),
    {ok, State};

run(pb_encode, _KeyGen, _ValueGen, State) ->
    {ok, Expected} = State#state.expected,
    Msg = #sequenceresponse { sequence = Expected },
    [MsgType, MsgBytes1] = protobuff_server:encode(Msg),
    Msg2 = protobuff_server:decode([MsgType|iolist_to_binary(MsgBytes1)]),
    Actual = Msg2#sequenceresponse.sequence,
    assertEqual(Expected, Actual),
    {ok, State};

run(Other, _KeyGen, _ValueGen, _State) ->
    io:format("[~s:~p] DEBUG - {unknown_operation, Other}: ~p~n", [?MODULE, ?LINE, {unknown_operation, Other}]),
    throw({unknown_operation, Other}).

ensure_module(Module) ->
    case code:which(Module) of
        non_existing ->
            throw({missing_module, Module});
        _ ->
            ok
    end.

assertEqual(_Expected, _Actual) ->
    %% ?assertEqual(Expected, Actual),
    ok.
