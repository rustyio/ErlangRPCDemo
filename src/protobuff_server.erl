%% Erlang RPC Demo
%% Copyright (c) 2011 Rusty Klophaus (@rustyio)
%% See MIT-LICENSE for licensing information.

-module(protobuff_server).
-behaviour(gen_server).

-export([
         start_link/0,
         set_socket/2,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         encode/1,
         decode/1]).

-record(state, { sock }).

-include("rpc_demo_pb.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

set_socket(Pid, Socket) ->
    gen_server:call(Pid, {set_socket, Socket}).

init([]) ->
    {ok, #state{}}.

handle_call({set_socket, Socket}, _From, State) ->
    inet:setopts(Socket, [{active, once}, {packet, 4}, {header, 1}]),
    {reply, ok, State#state{sock = Socket}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp_closed, Socket}, State=#state{sock=Socket}) ->
    {stop, normal, State};
handle_info({tcp_error, Socket, _Reason}, State=#state{sock=Socket}) ->
    {stop, normal, State};
handle_info({tcp, _Sock, MsgData}, State=#state{sock=Socket}) ->
    Msg = decode(MsgData),
    case process_message(Msg, State) of
        {pause, NewState} ->
            ok;
        NewState ->
            inet:setopts(Socket, [{active, once}])
    end,
    {noreply, NewState};
handle_info({tcp, _Sock, _Data}, State) ->
    %% req =/= undefined: received a new request while another was in
    %% progress -> Error
    lager:error("Received a new PB socket request"
                " while another was in progress"),
    {stop, normal, State};

handle_info(_, State) -> % Ignore any late replies from gen_servers/messages from fsms
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ===================================================================
%% Handle PB Messages
%% ===================================================================

process_message(#sequencerequest { n = N }, State) ->
    case sequence:sequence(N) of
        {ok, List} ->
            Resp = #sequenceresponse { sequence = List },
            send_msg(Resp, State);
        {error, Reason} ->
            Msg = io_lib:format("~w", [Reason]),
            Resp = #sequenceerror { message=Msg },
            send_msg(Resp, State)
    end.

%% Send a message to the client
send_msg(Msg, State) ->
    Pkt = encode(Msg),
    gen_tcp:send(State#state.sock, Pkt),
    State.

encode(Msg) ->
    MsgType = element(1, Msg),
    [msg_code(Msg) | rpc_demo_pb:iolist(MsgType, Msg)].

decode([MsgCode|MsgData]) ->
    MsgType = msg_type(MsgCode),
    rpc_demo_pb:decode(MsgType, MsgData).

msg_code(#sequencerequest {})  -> 1;
msg_code(#sequenceresponse {}) -> 2;
msg_code(#sequenceerror {})    -> 3;
msg_code(Other) ->
    throw({unknown_pb_type, Other}).

msg_type(1) -> sequencerequest;
msg_type(2) -> sequenceresponse;
msg_type(3) -> sequenceerror;
msg_type(Other) ->
    throw({unknown_pb_code, Other}).
