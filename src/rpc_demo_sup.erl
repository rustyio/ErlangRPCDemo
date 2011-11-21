
-module(rpc_demo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    application:start(inets),

    %% Start up Webmachine...
    application:load(webmachine),
    Options = [
        {ip, "0.0.0.0"},
        {port, 8001},
        {dispatch, dispatch()}
    ],
    webmachine_mochiweb:start(Options),

    %% Start up spooky...
    spooky:start_link(spooky_sequence),

    %% Start up ernie...
    application:start(ernie_server),

    Children = [
        {protobuff_server_sup, {protobuff_server_sup, start_link, []},
            permanent, infinity, supervisor, [protobuff_server_sup]},
        {protobuff_server_listener, {protobuff_server_listener, start_link, []},
            permanent, 5000, worker, [protobuff_server_listener]}
    ],
    {ok, { {one_for_one, 5, 10}, Children}}.

dispatch() ->
    [
        {["sequence", n],
            webmachine_sequence, []}
    ].
