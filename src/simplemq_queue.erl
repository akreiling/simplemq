%% @author Andrew Kreiling <akreiling@pobox.com>
%% @author Brian Smith
%% @copyright 2008 Andrew Kreiling, Brian Smith. All Rights Reserved.
%%
%% @doc
%% A gen_server
%%
-module(simplemq_queue).
-author('Andrew Kreiling <akreiling@pobox.com>').
-author('Brian Smith').
-behaviour(gen_server).

%% External exports
-export([start/1, start_link/1, stop/1]).
-export([subscribe/2, unsubscribe/2, send/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {destination, name, subscribers = pqueue:empty()}).
-define(state, State#state).

-include("simplemq.hrl").

%% @spec start(ServerRef) -> {ok, Pid}
%% @doc API for starting a queue.
start(Args) ->
    supervisor:start_child(simplemq_queue_sup, [Args]).

%% @spec stop(ServerRef) -> ok
%% @doc API for stopping a queue.
stop(Pid) ->
    gen_server:cast(Pid, stop).

%% @spec find(Destination) -> {ok, Pid}
%% @doc API for finding/starting a queue.
find(Destination) ->
    Name = destination_to_name(Destination),
    case erlang:whereis(Name) of
        undefined ->
            {ok, Pid} = start(Destination),
            Pid;
        Pid ->
            Pid
    end.

%% @spec subscribe(Destination, From) -> ok
%% @doc API for subscribing to a queue.
subscribe(Destination, From) ->
    Pid = find(Destination),
    gen_server:cast(Pid, {subscribe, From}).

%% @spec unsubscribe(Destination, From) -> ok
%% @doc API for unsubscribing to a queue.
unsubscribe(Destination, From) ->
    Pid = find(Destination),
    gen_server:cast(Pid, {unsubscribe, From}).

%% @spec send(Destination, Body) -> ok
%% @doc API for sending a message to a queue.
send(Destination, Body) ->
    Pid = find(Destination),
    gen_server:cast(Pid, {send, Body}).

%% @spec start_link(Port) -> ServerRet
%% @doc API for starting a queue.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% @spec init([]) -> {ok, State}
%% @doc supervisor callback.
init(Destination) ->
    process_flag(trap_exit, true),
    Name = destination_to_name(Destination),
    register(Name, self()),
    ok = register_queue(Destination),
    ok = create_queue(Name),
    State = #state{
        destination = Destination,
        name = Name
    },
    ?log({started, Destination}),
    {ok, State}.

handle_call(Request, _From, State) ->
    ?log({handle_call, Request}),
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({subscribe, Pid}, State) ->
    Subscribers = pqueue:in(Pid, 0, ?state.subscribers),
    link(Pid),
    ?log({"S", pqueue:members(Subscribers)}),
    {noreply, State#state{ subscribers = Subscribers }};

handle_cast({unsubscribe, Pid}, State) ->
    Subscribers = pqueue:remove(Pid, ?state.subscribers),
    unlink(Pid),
    ?log({"S", pqueue:members(Subscribers)}),
    {noreply, State#state{ subscribers = Subscribers }};

handle_cast({send, Body}, State) ->
    ID = make_id(),
    case pqueue:peek(?state.subscribers) of
        undefined ->
            M = #message{ id = ID, body = Body },
            ok = store_message(?state.name, M);
        Pid ->
            case (catch gen_fsm:sync_send_all_state_event(Pid, {message, ?state.destination, ID, Body})) of
                ack ->
                    ok;
                ok ->
                    M = #message{ id = ID, status = "sent", body = Body },
                    ok = store_message(?state.name, M);
                _ ->
                    M = #message{ id = ID, status = "new", body = Body },
                    ok = store_message(?state.name, M)
        end
    end,
    {noreply, State};

handle_cast(Msg, State) ->
    ?log({handle_cast, Msg}),
    {noreply, State}.

handle_info({'EXIT', Pid, _}, State) ->
    ok = gen_server:cast(self(), {unsubscribe, Pid}),
    {noreply, State};

handle_info(Info, State) ->
    ?log({handle_info, Info}),
    {noreply, State}.

terminate(_Reason, State) ->
    ?log({stopped, ?state.destination}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%%
%%

register_queue(Destination) ->
    F = fun() ->
            case mnesia:read(queue, Destination, write) of
                [] ->
                    Queue = #queue{name = Destination},
                    mnesia:write(Queue),
                    ok;
                _ ->
                    ok
            end
    end,
    {atomic, ok} = mnesia:transaction(F),
    ok.

create_queue(Name) ->
    case mnesia:create_table(Name, [{type, ordered_set}, {disc_copies, [node()]}, {local_content, true}, {attributes, record_info(fields, message)}, {record_name, message}]) of
        {atomic, ok} -> ok;
        {aborted,{already_exists, _}} -> ok;
        Any ->
            ?error({create_queue, Any}),
            error
    end.

store_message(Name, M) ->
    F = fun() ->
            mnesia:write(Name, M, write)
    end,
    {atomic, ok} = mnesia:transaction(F),
    ok.

make_id() ->
    uuid:to_string(uuid:timestamp()).

destination_to_name(Destination) ->
    list_to_atom(atom_to_list(?MODULE) ++ "_" ++ url:encode(Destination)).

%% vim:sw=4:sts=4:ts=8:et
