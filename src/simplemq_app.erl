%% @author Andrew Kreiling <akreiling@pobox.com>
%% @author Brian Smith
%% @copyright 2008 Andrew Kreiling, Brian Smith. All Rights Reserved.
%%
%% @doc
%% Callbacks for the simplemq application
%%
-module(simplemq_app).
-author('Andrew Kreiling <akreiling@pobox.com>').
-author('Brian Smith').
-behaviour(application).

-export([start/2, stop/1]).

%% @spec start(Type, StartArgs) -> Result
%%
%% @doc
%% application callback
start(_StartType, _StartArgs) ->
    simplemq_sup:start_link().

%% @spec stop(State) -> ok
%% where
%%      State = term()
%%
%% @doc
%% application callback
stop(_State) ->
    ok.

%% vim:sw=4:sts=4:ts=4:et
