%% @author Andrew Kreiling <akreiling@pobox.com>
%% @author Brian Smith
%% @copyright 2008 Andrew Kreiling, Brian Smith. All Rights Reserved.
%%
%% @doc
%% Supervisor for the simplemq_queue.
%%
-module(simplemq_queue_sup).
-author('Andrew Kreiling <akreiling@pobox.com>').
-author('Brian Smith').
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
        {simplemq_queue,
            {simplemq_queue, start_link, []},
            temporary, 60000, worker, [simplemq_queue]}
    ],
    ?log(started),
    {ok, {{simple_one_for_one, 0, 1}, Processes}}.

%% vim:sw=4:sts=4:ts=8:et
