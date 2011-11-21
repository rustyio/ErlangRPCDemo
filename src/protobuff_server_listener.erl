-module(protobuff_server_listener).
-behavior(gen_nb_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([sock_opts/0, new_connection/2]).
-record(state, {portnum}).

start_link() ->
    IP = "0.0.0.0",
    Port = 8003,
    gen_nb_server:start_link(?MODULE, IP, Port, [Port]).

init([PortNum]) ->
    {ok, #state{portnum=PortNum}}.

sock_opts() ->
    [binary, {packet, 4}, {reuseaddr, true}, {backlog, 5}].

handle_call(_Req, _From, State) ->
    {reply, not_implemented, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

new_connection(Socket, State) ->
    Result = protobuff_server_sup:start_socket(),
    io:format("[~s:~p] DEBUG - Result: ~p~n", [?MODULE, ?LINE, Result]),
    {ok, Pid} = protobuff_server_sup:start_socket(),
    io:format("[~s:~p] DEBUG - Pid: ~p~n", [?MODULE, ?LINE, Pid]),
    ok = gen_tcp:controlling_process(Socket, Pid),
    io:format("[~s:~p] DEBUG - 1: ~p~n", [?MODULE, ?LINE, 1]),
    ok = protobuff_server:set_socket(Pid, Socket),
    io:format("[~s:~p] DEBUG - 2: ~p~n", [?MODULE, ?LINE, 2]),
    {ok, State}.
