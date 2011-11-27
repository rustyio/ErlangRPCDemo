%% Erlang RPC Demo
%% Copyright (c) 2011 Rusty Klophaus (@rustyio)
%% See MIT-LICENSE for licensing information.

-module(webmachine_sequence).

%% webmachine resource exports
-export([
         init/1,
         malformed_request/2,
         content_types_provided/2,
         to_json/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {
          list
         }).

init([]) ->
    {ok, #ctx {}}.

malformed_request(RD, Ctx) ->
    N = wrq:path_info(n, RD),
    case sequence:sequence(N) of
        {ok, List} ->
            {false, RD, Ctx#ctx { list=List }};
        {error, Reason} ->
            Msg = io_lib:format("~w", [Reason]),
            RD1 = wrq:set_response_body(Msg, RD),
            {true, RD1, Ctx}
    end.

content_types_provided(RD, Ctx) ->
    Types = [{"application/json", to_json}],
    {Types, RD, Ctx}.

to_json(RD, Ctx) ->
    List = Ctx#ctx.list,
    Body = mochijson2:encode(List),
    {Body, RD, Ctx}.
