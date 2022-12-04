#!/usr/bin/env swipl

:- use_module(io).
:- use_module(library(dcg/basics)).

% parsing
range((A, B)) --> integer(A), "-", integer(B).
pair((A, B)) --> range(A), ",", range(B).

parse(Str, Out) :-
    phrase(pair(Out), Str, []).

contains(((A, B), (C, D))) :-
    ( A =< C, D =< B
    ; C =< A, B =< D
    ).

overlap(((A, B), (C, D))) :-
    \+ (A < C -> B < C; D < A).

solve(Pred, Pairs) :-
    include(Pred, Pairs, L),
    length(L, N),
    writeln(N).

load(Data) :-
    file_lines("../input/d04", Lines),
    maplist(atom_codes, Lines, Codes),
    maplist(parse, Codes, Data).

main :-
    load(Data),
    solve(contains, Data),
    solve(overlap, Data).
