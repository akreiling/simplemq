%% @author Andrew Kreiling <akreiling@pobox.com>
%% @author Brian Smith
%% @copyright 2008 Andrew Kreiling, Brian Smith. All Rights Reserved.
%%
%% @doc
%% Supervisor for the simplemq application.
%%
-module(simplemq_sup).
-author('Andrew Kreiling <akreiling@pobox.com>').
-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-include("simplemq.hrl").

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Processes = [
        {uuid,
            {uuid, start_link, []},
            permanent, brutal_kill, supervisor, [uuid]},
        {simplemq_server,
            {simplemq_server, start_link, []},
            permanent, brutal_kill, supervisor, [simplemq_server]},
        {stomp_listener,
            {stomp_listener, start_link, [61613]},
            permanent, brutal_kill, supervisor, [stomp_listener]}
    ],
    ?log(started),
    {ok, {{one_for_one, 10, 10}, Processes}}.

%% vim:sw=4:sts=4:ts=8:et
