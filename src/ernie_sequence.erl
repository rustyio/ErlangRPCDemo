%% Erlang RPC Demo
%% Copyright (c) 2011 Rusty Klophaus (@rustyio)
%% See MIT-LICENSE for licensing information.

-module(ernie_sequence).
-export([sequence/1]).

sequence(N) ->
    sequence:sequence(N).
