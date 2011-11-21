-module(protobuff_server_sup).
-behaviour(supervisor).
-export([start_link/0, init/1, stop/1]).
-export([start_socket/0]).

start_socket() ->
    supervisor:start_child(?MODULE, []).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_S) -> ok.

%% @private
init([]) ->
    {ok,
     {{simple_one_for_one, 10, 10},
      [{undefined,
        {protobuff_server, start_link, []},
        temporary, brutal_kill, worker, [protobuff_server]}]}}.
