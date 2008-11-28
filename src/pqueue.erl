-module(pqueue).
-export([]).
-compile(export_all).

empty() ->
    gb_trees:empty().

in(T, P, Q) ->
    case member(T, Q) of
        true -> Q;
        false -> gb_trees:insert({P, now()}, T, Q)
    end.

out(Q) ->
    gb_trees:take_smallest(Q).

peek(Q) ->
    case gb_trees:is_empty(Q) of
        true ->
            undefined;
        false ->
            {_, V} = gb_trees:smallest(Q),
            V
    end.

member(T, Q) ->
    case find(T, Q) of
        none -> false;
        _ -> true
    end.

members(Q) ->
    gb_trees:values(Q).

find(T, Q) ->
    I = gb_trees:iterator(Q),
    find_next(T, I).

find_next(T, I) ->
    case gb_trees:next(I) of
        {K, V, I2} ->
            if
                (V == T) -> K;
                true -> find_next(T, I2)
            end;
        none -> none
    end.

remove(T, Q) ->
    case find(T, Q) of
        none ->
            Q;
        K ->
            gb_trees:delete(K, Q)
    end.

test() ->
    Q1 = empty(),
    Q2 = in(a, 1, Q1),
    Q3 = in(b, 2, Q2),
    Q4 = in(c, 3, Q3),
    Q = in(c, 4, Q4),
    Q.

%% vim:sw=4:sts=4:ts=4:et
