%% @author Andrew Kreiling <akreiling@pobox.com>
%% @author Brian Smith
%% @copyright 2008 Andrew Kreiling, Brian Smith. All Rights Reserved.
%%
-module(simplemq).
-author('Andrew Kreiling <akreiling@pobox.com>').
-author('Brian Smith').
-export([start/0, stop/0]).

-include("simplemq.hrl").

ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok;
        Any -> Any
    end.

%% @spec start() -> Result
%%
%% @doc
%% Start the simplemq application
start() ->
    ok = ensure_started(crypto),
    ok = ensure_started(mnesia),
    %% ok = mnesia:wait_for_tables([queue], infinity),
    application:start(simplemq).

%% @spec stop() -> Result
%%
%% @doc
%% Stop the simplemq appliction.
stop() ->
    application:stop(simplemq).

%% vim:sw=4:sts=4:ts=8:et
