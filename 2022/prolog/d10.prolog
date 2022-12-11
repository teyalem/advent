:- use_module(io).
:- use_module(library(dcg/basics)).

noop --> "noop".
addx(N) --> "addx", whites, integer(N).

next_state({Cycle, X}, Cmd, Next) :-
    ( phrase(noop, Cmd) ->
        Ncycle is Cycle + 1,
        Nx is X
    ; phrase(addx(N), Cmd) ->
        Ncycle is Cycle + 2,
        Nx is X + N
    ),
    Next = {Ncycle, Nx}.

states(State, [], [State]).
states(State, [Cmd|Rest], States) :-
    next_state(State, Cmd, Next),
    states(Next, Rest, NextStates),
    States = [State|NextStates].

% make noncontiguous cycle contiguous
streamline(_State, [], []).
streamline({Cycle, X}, [{NewCycle, NewX}|Rest], Out) :-
    Cycle2 is Cycle + 1, 
    ( Cycle =:= NewCycle -> X2 = NewX, States = Rest
    ; X2 = X, States = [{NewCycle, NewX}|Rest]
    ),
    streamline({Cycle2, X2}, States, Out2),
    Out = [{Cycle, X2} | Out2].

streamline([First|Rest], Out) :-
    streamline(First, Rest, Out).

machine(Cmds, Logs) :-
    states({1, 1}, Cmds, States),
    streamline(States, Logs).

load(Data) :-
    file_lines("../input/d10", Lines),
    maplist(atom_codes, Lines, Data).

solve1(Data) :-
    machine(Data, M),
    maplist([X, O]>>(O is 20 + 40*X), [0, 1, 2, 3, 4, 5], Cycles),
    maplist({M}/[X, O]>>nth1(X, M, O), Cycles, Xs),
    maplist([{C, X}, O]>>(O is C*X), Xs, SignalStrengths),
    foldl(plus, SignalStrengths, 0, N),
    writeln(N).

split_equal(_N, [], []).
split_equal(N, Xs, Out) :-
    length(L, N),
    append(L, Rest, Xs),
    split_equal(N, Rest, Out2),
    Out = [L|Out2].

display(States, Display) :-
    maplist([{C, X}, O]>>
    ( 
        Cur is (C - 1) mod 40,
        L is X - 1,
        R is X + 1,
        (between(L, R, Cur) -> O = '#' ; O = ' ')
    ),
    States, Out),
    length(D, 240),
    append(D, _, Out),
    split_equal(40, D, Display).

solve2(Data) :-
    machine(Data, M),
    display(M, Display),
    maplist([L]>>(maplist(write, L), nl), Display).

main :-
    load(Data),
    solve1(Data),
    solve2(Data).
