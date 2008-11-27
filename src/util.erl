-module(util).
-export([binary_to_hex/1, nibble_to_hex/1]).

binary_to_hex(B) ->
    [ nibble_to_hex(N) || <<N:4>> <= B ].

nibble_to_hex(N) when N < 10 ->
    $0 + N;

nibble_to_hex(N) when N >= 10, N < 16 ->
    $a + N - 10.

%% vim:sw=4:sts=4:ts=4:et
