%% @author Andrew Kreiling <akreiling@pobox.com>
%% @author Brian Smith
%% @copyright 2008 Andrew Kreiling, Brian Smith. All Rights Reserved.
%%
%% @doc
%% gen_tcp listener
%%
-module(stomp_listener).
-author('Andrew Kreiling <akreiling@pobox.com>').
-author('Brian Smith').
-behaviour(gen_listener).

%% External exports
-export([start_link/1, init/1, handle_accept/1]).

-include("simplemq.hrl").

start_link(Port) ->
    gen_listener:start_link(?MODULE, Port).

init(Port) ->
    State = Port,
    {ok, State}.

handle_accept(Socket) ->
    {ok, Pid} = stomp_gateway:start(Socket),
    ok = gen_tcp:controlling_process(Socket, Pid),
    stomp_gateway:activate(Pid).

%% vim:sw=4:sts=4:ts=4:et
