%% @author Andrew Kreiling <akreiling@pobox.com>
%% @author Brian Smith
%% @copyright 2008 Andrew Kreiling, Brian Smith. All Rights Reserved.
%%
-define(log(Term), error_logger:info_report([{module, ?MODULE}, {pid, self()}, Term])).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% vim:ft=erlang:sw=4:sts=4:ts=4:et
