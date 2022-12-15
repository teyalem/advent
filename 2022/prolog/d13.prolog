:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

packets([]) --> [].
packets([X|L]) --> pair(X), eol, eol, packets(L).
pair({A, B}) --> list(A), eol, list(B).
list(L) --> sequence("[", element, ",",  "]", L).
element(X) --> (integer(X); list(X)).

comp_list((=), [], []).
comp_list((<), [], _Bs).
comp_list((>), _As, []).
comp_list(C, [A|As], [B|Bs]) :-
    comp(C1, A, B), !,
    ( C1 == (=) -> comp_list(C, As, Bs)
    ; C = C1
    ).

comp(C, A, B) :-
    ( integer(A), integer(B) -> compare(C, A, B)
    ; integer(A), is_list(B) -> comp_list(C, [A], B)
    ; is_list(A), integer(B) -> comp_list(C, A, [B])
    ; is_list(A), is_list(B) -> comp_list(C, A, B)
    ).

load(Data) :-
    File = "../input/d13",
    phrase_from_file(packets(Data), File).

solve1(Data) :-
    length(Data, Len),
    bagof(N, between(1, Len, N), Is),
    maplist([I, {A, B}, O]>>(comp((<), A, B) -> O = I ; O = none), Is, Data, List),
    exclude([N]>>(N = none), List, Indexes),
    foldl(plus, Indexes, 0, N),
    writeln(N).

decoder_index(P, Ps, N) :-
    include({P}/[O]>>comp((<), O, P), Ps, Pre),
    length(Pre, N1),
    N is N1 + 1.

solve2(Data) :-
    maplist([{A, B}, O]>>(O = [A, B]), Data, P2),
    append(P2, Packets),
    phrase(list(A), `[[2]]`),
    phrase(list(B), `[[6]]`),
    decoder_index(A, Packets, Na),
    decoder_index(B, Packets, Nb),
    N is Na * (Nb + 1),
    writeln(N).

main :-
    load(Data),
    solve1(Data),
    solve2(Data).
