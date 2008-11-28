%% @author Andrew Kreiling <akreiling@pobox.com>
%% @author Brian Smith
%% @copyright 2008 Andrew Kreiling, Brian Smith. All Rights Reserved.
%%
-define(debug(Args), logger:debug(?FILE, ?LINE, Args)).
-define(log(Args), logger:info(?MODULE, Args)).
-define(warn(Args), logger:warn(?MODULE, Args)).
-define(error(Args), logger:error(?MODULE, Args)).

-record(queue, {name, messages_in = 0, messages_out = 0}).
-record(message, {id, status = "new", body}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% vim:ft=erlang:sw=4:sts=4:ts=4:et
