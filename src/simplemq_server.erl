%% @author Andrew Kreiling <akreiling@pobox.com>
%% @author Brian Smith
%% @copyright 2008 Andrew Kreiling, Brian Smith. All Rights Reserved.
%%
%% @doc
%% A gen_server
%%
-module(simplemq_server).
-author('Andrew Kreiling <akreiling@pobox.com>').
-author('Brian Smith').
-behaviour(gen_server).

%% External exports
-export([start_link/0, stop/0]).
-export([auth/2, subscribe/1, send/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).
-define(state, State#state).

-include("simplemq.hrl").

%% @spec start_link() -> Result
%%
%% @doc
%% start the simplemq_server
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec stop() -> ok
%%
%% @doc
%% stop the simplemq_server
%%
stop() ->
    gen_server:cast(?MODULE, stop).

%% @spec auth(Login, Password) -> ok
%%
%% @doc
%% stop the simplemq_server
%%
auth(_Login, _Password) ->
    ok.

%%
subscribe(Destination) ->
    gen_server:cast(?MODULE, {subscribe, self(), Destination}).

%%
send(Destination, Body) ->
    gen_server:cast(?MODULE, {send, Destination, Body}).

%% @spec init(Args) -> Result
%% where
%%      Args = term()
%%
%% @doc
%% gen_server callback
%%
init(_Args) ->
    process_flag(trap_exit, true),
    ok = ensure_schema(),
    ok = ensure_tables(),
    State = #state{},
    ?log(started),
    {ok, State}.

%% @spec handle_call(Request, From, State) -> Result
%% where
%%      Request = term()
%%      From = {pid(), Tag}
%%      State = term()
%%
%% @doc
%% gen_server callback
%%
handle_call(Request, _From, State) ->
    ?log({handle_call, Request}),
    {reply, ok, State}.

%% @spec handle_cast(Request, State) -> Result
%% where
%%      Request = term()
%%      State = term()
%%
%% @doc
%% gen_server callback
%%
handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({subscribe, From, Destination}, State) ->
    simplemq_queue:subscribe(Destination, From),
    {noreply, State};

handle_cast({send, Destination, Body}, State) ->
    simplemq_queue:send(Destination, Body),
    {noreply, State};

handle_cast(Request, State) ->
    ?log({handle_cast, Request}),
    {noreply, State}.

%% @spec handle_info(Info, State) -> Result
%% where
%%      Info = timeout | term()
%%      State = term()
%%
%% @doc
%% gen_server callback
%%
handle_info(Info, State) ->
    ?log({handle_info, Info}),
    {noreply, State}.

%% @spec terminate(Reason, State) -> ok
%% where
%%      Reason = normal | shutdown | term()
%%      State = term()
%%
%% @doc
%% gen_server callback
%%
terminate(_Reason, _State) ->
    ?log(stopped),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% where
%%      OldVsn = Vsn | {down, Vsn}
%%      Vsn = term()
%%      State = term()
%%      Extra = term()
%%      NewState = term()
%%
%% @doc
%% gen_server callback
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%%
%%

ensure_schema() ->
    case mnesia:change_table_copy_type(schema, node(), disc_copies) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, _, _, _}} -> ok;
        Any ->
            ?error({ensure_schema, Any}),
            error
    end.

ensure_tables() ->
    case mnesia:create_table(queue, [{disc_only_copies, [node()]}, {attributes, record_info(fields, queue)}]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, _}} -> ok;
        Any ->
            ?error({create_tables, Any}),
            error
    end.

%% vim:sw=4:sts=4:ts=8:et
