:- use_module(io).
:- use_module(library(dcg/basics)).

pull_col(In, Index, Out) :-
    maplist(nth0(Index), In, Xs),
    exclude([X]>>(X =:= 32), Xs, Out).

pull_stack(In, Stack) :-
    maplist(length, In, Lens),
    foldl([A, B, C]>>(C is max(A, B)), Lens, 0, L),
    Nstack is div(L, 4),
    numlist(0, Nstack, Ns),
    maplist([X, Y]>>(Y is 1 + 4*X), Ns, Index),
    maplist(pull_col(In), Index, Stack).

com((N, F, T)) -->
    "move", whites, integer(N), whites,
    "from", whites, integer(F), whites,
    "to", whites, integer(T).

parse(Slist, Stack, Com) :-
    append(Stext, [_, []|Ctext], Slist),
    pull_stack(Stext, Stack),
    maplist([Line, C]>>phrase(com(C), Line, []), Ctext, Com).

move(Part2, (N, F, T), Stacks, NewStacks) :-
    % pull head and tail
    nth1(F, Stacks, Source),
    length(Head, N),
    append(Head, Tail, Source),
    (Part2 -> NewHead = Head ; reverse(Head, NewHead)),
    % fill index list
    length(Stacks, Len),
    numlist(1, Len, Index),
    % produce new stacks
    maplist({NewHead, Tail}/[I, Stack, NewStack]>>
    ( I =:= F -> NewStack = Tail
    ; I =:= T -> append(NewHead, Stack, NewStack)
    ; NewStack = Stack
    ),
    Index, Stacks, NewStacks).

load(Stacks, Com) :-
    file_lines("../input/d05", Lines),
    maplist(atom_codes, Lines, Codes),
    parse(Codes, Stacks, Com).

collect_first(Stacks, Firsts) :-
    maplist([[X|_], Y]>>(Y = X), Stacks, Firsts).

solve(Part2, Stacks, Com) :-
    foldl(move(Part2), Com, Stacks, NewStacks),
    collect_first(NewStacks, Codes),
    atom_codes(A, Codes),
    writeln(A).

main :-
    load(Stacks, Com),
    solve(false, Stacks, Com),
    solve(true, Stacks, Com).
