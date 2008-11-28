-module(url).
-export([encode/1]).

encode(S) ->
    encode(S, []).

encode([H|T], Acc) when H >= $a, H =< $z ->
    encode(T, [H|Acc]);

encode([H|T], Acc) when H >= $A, H =< $Z ->
    encode(T, [H|Acc]);

encode([H|T], Acc) when H == $-; H == $_; H == $. ->
    encode(T, [H|Acc]);

encode([32|T], Acc) ->
    encode(T, [$+|Acc]);

encode([H|T], Acc) ->
    encode(T, [util:nibble_to_hex(H band 16#0f), util:nibble_to_hex(H bsr 4), $% | Acc]);

encode([], Acc) ->
    lists:reverse(Acc).

%% vim:sw=4:sts=4:ts=8:et
