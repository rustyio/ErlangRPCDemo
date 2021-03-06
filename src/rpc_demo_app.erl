%% Erlang RPC Demo
%% Copyright (c) 2011 Rusty Klophaus (@rustyio)
%% See MIT-LICENSE for licensing information.

-module(rpc_demo_app).

-behaviour(application).

%% Application callbacks
-export([
         start/2,
         stop/1
        ]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    rpc_demo_sup:start_link().

stop(_State) ->
    ok.
