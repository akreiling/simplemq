%% @author Andrew Kreiling <akreiling@pobox.com>
%% @copyright 2008 Andrew Kreiling. All Rights Reserved.
%%
%% @doc
%% gen_tcp listener
%%
-module(gen_listener).
-author('Andrew Kreiling <akreiling@pobox.com>').

%% callbacks for behaviour
-export([behaviour_info/1]).

%% External exports
-export([start/2, start_link/2, init/2]).

behaviour_info(callbacks) ->
    [{handle_accept, 1}];
behaviour_info(_Other) ->
    undefined.

start(Mod, Port) ->
    Pid = spawn(?MODULE, init, [Mod, Port]),
    {ok, Pid}.

start_link(Mod, Port) ->
    Pid = spawn_link(?MODULE, init, [Mod, Port]),
    {ok, Pid}.

init(Mod, Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {backlog, 30}, {active, false}, {packet, raw}, {reuseaddr, true}]),
    loop(Mod, ListenSocket).

loop(Mod, ListenSocket) ->
    case gen_tcp:accept(ListenSocket, 5000) of
        {ok, Socket} ->
            Mod:handle_accept(Socket),
            loop(Mod, ListenSocket);
        {error, timeout} ->
            loop(Mod, ListenSocket);
        {error, Reason} ->
            {error, Reason}
    end.

%% vim:sw=4:sts=4:ts=8:et
