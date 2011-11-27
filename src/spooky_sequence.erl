%% Erlang RPC Demo
%% Copyright (c) 2011 Rusty Klophaus (@rustyio)
%% See MIT-LICENSE for licensing information.

-module(spooky_sequence).
-behaviour(spooky).
-export([init/1, get/2]).

init([])->
    [{port, 8002}].

get(_Req, ["sequence", Num])->
    case sequence:sequence(Num) of
        {ok, List} ->
            {200, mochijson2:encode(List)};
        {error, Error} ->
            {500, io_lib:format("~w", [Error])}
    end;
get(_Req, _)->
    {400, "Usage: /sequence/:Num:"}.
