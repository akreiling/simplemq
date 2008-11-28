%% @author Andrew Kreiling <akreiling@pobox.com>
%% @copyright 2008 Andrew Kreiling. All Rights Reserved.

-module(logger).
-author('Andrew Kreiling <akreiling@pobox.com>').

-export([debug/3, info/2, warn/2, error/2]).

debug(File, Line, Args) ->
    io:format("DEBUG ~s:~b => ~p~n", [File, Line, Args]).

info(Mod, Args) when is_list(Args) ->
    error_logger:info_report(lists:flatten([{module, Mod}, {pid, self()}, [Args]]));

info(Mod, Args) ->
    info(Mod, [Args]).

warn(Mod, Args) when is_list(Args) ->
    error_logger:warning_report(lists:flatten([{module, Mod}, {pid, self()}, [Args]]));

warn(Mod, Args) ->
    warn(Mod, [Args]).

error(Mod, Args) when is_list(Args) ->
    error_logger:error_report(lists:flatten([{module, Mod}, {pid, self()}, [Args]]));

error(Mod, Args) ->
    error(Mod, [Args]).

%% vim:sw=4:sts=4:ts=8:et
