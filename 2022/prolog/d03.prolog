:- module(d03, [main/0]).
:- use_module(io).

split_half(List, First, Second) :-
    length(List, L),
    H is L / 2,
    length(First, H), length(Second, H),
    append(First, Second, List).

priority(C, N) :-
    atom_codes("azAZ", [A, Z, A2, Z2]),
    ( (A =< C, C =< Z) -> N is C - A + 1
    ; (A2 =< C, C =< Z2) -> N is C - A2 + 27
    ).

same_item(Sack, X) :-
    split_half(Sack, A, B),
    list_to_set(A, S1),
    list_to_set(B, S2),
    intersection(S1, S2, [X]).

solve1(Data) :-
    maplist(same_item, Data, Items),
    maplist(priority, Items, Ns),
    foldl(plus, Ns, 0, N),
    writeln(N).

group3([], []).
group3([A, B, C | Rest], [[A, B, C] | Out]) :-
    group3(Rest, Out).

badge([A, B, C], X) :-
    list_to_set(A, S1),
    list_to_set(B, S2),
    list_to_set(C, S3),
    intersection(S1, S2, S),
    intersection(S, S3, [X]).

solve2(Data) :-
    group3(Data, Groups),
    maplist(badge, Groups, Badges),
    maplist(priority, Badges, Ns),
    foldl(plus, Ns, 0, N),
    writeln(N).

load(Data) :-
    file_lines("../input/d03", Lines),
    maplist(atom_codes, Lines, Data).

main :-
    load(Data),
    solve1(Data),
    solve2(Data).
