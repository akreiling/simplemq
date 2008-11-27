%% @author Andrew Kreiling <akreiling@pobox.com>
%% @author Brian Smith
%% @copyright 2008 Andrew Kreiling, Brian Smith. All Rights Reserved.
%%
%% @doc
%% A gen_fsm
%%
-module(stomp_gateway).
-author('Andrew Kreiling <akreiling@pobox.com>').
-author('Brian Smith').
-behaviour(gen_fsm).

%% External exports
-export([start/1, start_link/1, stop/1, activate/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([wait_for_activate/2, wait_for_command/2, wait_for_headers/2, wait_for_body/2]).

-record(frame, {command = [], headers = [], body = []}).
-define(frame, Frame#frame).
-record(state, {timeout = 5000, socket, frames_in = 0, frames_out = 0, session, frame = #frame{}, buffer = []}).
-define(state, State#state).

-include("simplemq.hrl").

%% @spec start(Socket) -> Result
%% where
%%      Socket = socket()
%%
%% @doc
%% start a stomp_gateway
%%
start(Socket) ->
    gen_fsm:start(?MODULE, Socket, []).

%% @spec start_link(Socket) -> Result
%% where
%%      Socket = socket()
%%
%% @doc
%% start a stomp_gateway
%%
start_link(Socket) ->
    gen_fsm:start_link(?MODULE, Socket, []).

%% @spec stop(Pid) -> ok
%% where
%%      Pid = pid()
%%
%% @doc
%% stop a stomp_gateway
%%
stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).

%% @spec activate(Pid) -> ok
%% where
%%      Pid = pid()
%%
%% @doc
%% activate the stomp_gateway
%%
activate(Pid) ->
    gen_fsm:send_event(Pid, activate).

%% @spec init(Socket) -> Result
%% where
%%      Socket = socket()
%%
%% @doc
%% gen_fsm callback
%%
init(Socket) ->
    State = #state{ socket = Socket },
    ?log([started]),
    {ok, wait_for_activate, State}.

%% @spec handle_event(Event, StateName, State) -> Result
%% where
%%      Event = term()
%%      StateName = atom()
%%      State = term()
%%
%% @doc
%% gen_fsm callback
%%
handle_event(stop, _StateName, State) ->
        {stop, normal, State};

handle_event({error, Reason, _Frame}, _StateName, State) ->
    F = #frame{
        command = "ERROR",
        headers = [{"message", Reason}]
    },
    ok = send_frame(?state.socket, F),
    {stop, Reason, State};

handle_event(#frame{ command = "CONNECT", headers = Headers } = Frame, StateName, State) ->
    Session = util:binary_to_hex(uuid:random()),
    Login = proplists:get_value("login", Headers),
    Passcode = proplists:get_value("passcode", Headers),
    case simplemq_server:auth(Login, Passcode) of
        ok ->
            F = #frame{
                command = "CONNECTED",
                headers = [{"session", Session}]
            },
            ok = send_frame(?state.socket, F),
            {next_state, StateName, State#state{ session = Session }};
        _ ->
            gen_fsm:send_all_state_event(self(), {error, auth_failed, Frame}),
            {next_state, StateName, State}
    end;

handle_event(#frame{ command = "SUBSCRIBE", headers = Headers } = Frame, StateName, State) ->
    {next_state, StateName, State};

handle_event(#frame{ command = "SEND", headers = Headers } = Frame, StateName, State) ->
    {next_state, StateName, State};

handle_event(Event, StateName, State) ->
    ?log([{state_name, StateName}, {handle_event, Event}]),
    {next_state, StateName, State}.

%% @spec handle_sync_event(Event, From, StateName, State) -> Result
%% where
%%      Event = term()
%%      From = pid()
%%      StateName = atom()
%%      State = term()
%%
%% @doc
%% gen_fsm callback
%%
handle_sync_event(Event, _From, StateName, State) ->
    ?log([{state_name, StateName}, {handle_sync_event, Event}]),
    {reply, error, StateName, State}.

%% @spec handle_info(Info, StateName, State) -> Result
%% where
%%      Info = timeout | term()
%%      StateName = atom()
%%      State = term()
%%
%% @doc
%% gen_fsm callback
%%
handle_info({tcp, _Socket, Data}, StateName, State) ->
    gen_fsm:send_event(self(), {tcp, Data}),
    {next_state, StateName, State};

handle_info({tcp_closed, _Socket}, _StateName, State) ->
    {stop, normal, State};

handle_info({tcp_error, _Socket, _Reason}, _StateName, State) ->
    {stop, normal, State};

handle_info(Info, StateName, State) ->
    ?log([{state_name, StateName}, {handle_info, Info}]),
    {next_state, StateName, State}.

%% @spec terminate(Reason, StateName, State) -> ok
%% where
%%      Reason = normal | shutdown | term()
%%      StateName = atom()
%%      State = term()
%%
%% @doc
%% gen_fsm callback
%%
terminate(_Reason, _StateName, _State) ->
    ?log(stopped),
    ok.

%% @spec code_change(OldVsn, StateName, State, Extra) -> Result
%% where
%%      OldVsn = term()
%%      StateName = atom()
%%      State = term()
%%      Extra = term()
%%
%% @doc
%% gen_fsm callback
%%
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%
%%
%%

%% @spec wait_for_activate(Event, State) -> Result
%%
%% @doc
%% gen_fsm callback
%%
wait_for_activate(activate, State) ->
    inet:setopts(?state.socket, [{active, once}]),
    {next_state, wait_for_command, State, ?state.timeout}.

%% @spec wait_for_command(Event, State) -> Result
%%
%% @doc
%% gen_fsm callback
%%
wait_for_command({tcp, Packet}, #state{ frame = Frame, buffer = Buffer } = State) ->
    case unpack_command(Packet, Buffer) of
        {ok, Command, Acc} ->
            NewState = State#state{
                frame = Frame#frame{ command = Command },
                buffer = []
            },
            gen_fsm:send_event(self(), {tcp, Acc}),
            {next_state, wait_for_headers, NewState};
        {incomplete, Acc} ->
            NewState = State#state{
                buffer = Acc
            },
            inet:setopts(?state.socket, [{active, once}]),
            {next_state, wait_for_command, NewState, ?state.timeout}
    end;

wait_for_command(timeout, State) ->
    ?log({wait_for_command, timeout}),
    {stop, normal, State}.

%% @spec wait_for_headers(Event, State) -> Result
%%
%% @doc
%% gen_fsm callback
%%
wait_for_headers({tcp, Packet}, #state{ frame = Frame, buffer = Buffer } = State) ->
    case unpack_headers(Packet, ?frame.headers, Buffer) of
        {ok, Headers, Acc} ->
            NewState = State#state{
                frame = Frame#frame{ headers = Headers },
                buffer = []
            },
            gen_fsm:send_event(self(), {tcp, Acc}),
            {next_state, wait_for_body, NewState};
        {incomplete, Headers, Acc} ->
            NewState = State#state{
                frame = Frame#frame{ headers = Headers },
                buffer = Acc
            },
            inet:setopts(?state.socket, [{active, once}]),
            {next_state, wait_for_headers, NewState, ?state.timeout}
    end;

wait_for_headers(timeout, State) ->
    ?log({wait_for_headers, timeout}),
    {stop, normal, State}.

%% @spec wait_for_body(Event, State) -> Result
%%
%% @doc
%% gen_fsm callback
%%
wait_for_body({tcp, Packet}, #state{ frame = Frame, buffer = Buffer } = State) ->
    case unpack_body(Packet, Buffer) of
        {ok, Body, Acc} ->
            NewState = State#state{
                frame = #frame{},
                buffer = []
            },
            gen_fsm:send_all_state_event(self(), Frame#frame{ body = Body }),
            gen_fsm:send_event(self(), {tcp, Acc}),
            {next_state, wait_for_command, NewState};
        {incomplete, Acc} ->
            NewState = State#state{
                buffer = Acc
            },
            inet:setopts(?state.socket, [{active, once}]),
            {next_state, wait_for_body, NewState, ?state.timeout}
    end;

wait_for_body(timeout, State) ->
    ?log({wait_for_body, timeout}),
    {stop, normal, State}.

%%
%%
%%

unpack_command(<<>>, Acc) ->
    {incomplete, Acc};

unpack_command(<<$\n, T/binary>>, Acc) ->
    {ok, lists:reverse(Acc), T};

unpack_command(<<$\r, T/binary>>, Acc) ->
    unpack_command(T, Acc);

unpack_command(<<H, T/binary>>, Acc) ->
    unpack_command(T, [H|Acc]).

unpack_headers(<<>>, Headers, Acc) ->
    {incomplete, Headers, Acc};

unpack_headers(<<$\n, T/binary>>, Headers, []) ->
    {ok, lists:keysort(1, Headers), T};

unpack_headers(<<$\n, T/binary>>, Headers, Acc) ->
    Header = parse_header(Acc),
    unpack_headers(T, [Header|Headers], []);

unpack_headers(<<$\r, T/binary>>, Headers, Acc) ->
    unpack_headers(T, Headers, Acc);

unpack_headers(<<H, T/binary>>, Headers, Acc) ->
    unpack_headers(T, Headers, [H|Acc]).

parse_header(H) ->
    P = string:chr(H, $:),
    {
        lists:reverse(string:to_lower(string:substr(H, P + 1))),
        lists:reverse(string:strip(string:substr(H, 1, P - 1)))
    }.

unpack_body(<<>>, Acc) ->
    {incomplete, Acc};

unpack_body(<<0, T/binary>>, Acc) ->
    {ok, lists:reverse(Acc), T};

unpack_body(<<H, T/binary>>, Acc) ->
    unpack_body(T, [H|Acc]).

build_frame(#frame{ headers = undefined } = Frame)->
    build_frame(Frame#frame{ headers = [] });

build_frame(#frame{ body = undefined } = Frame)->
    build_frame(Frame#frame{ body = <<>> });

build_frame(#frame{ body = Body } = Frame) when is_list(Body) ->
    build_frame(Frame#frame{ body = list_to_binary(Body) });

build_frame(Frame) ->
    ServerHeaders = [
        {"content-length", integer_to_list(size(?frame.body))},
        {"content-type", "text/plain; charset=UTF-8"}
    ],
    HHeaders = keymerge(ServerHeaders, ?frame.headers),
    [
        ?frame.command, $\n,
        [ [K, $:, V, $\n] || {K, V} <- HHeaders ], $\n,
        ?frame.body, $\0
    ].

send_frame(Socket, Frame) ->
    gen_tcp:send(Socket, build_frame(Frame)).

keymerge(L1, L2) ->
    lists:keymerge(1, lists:keysort(1, L1), lists:keysort(1, L2)).

-ifdef(EUNIT).
-endif.

%% vim:sw=4:sts=4:ts=4:et
