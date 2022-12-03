% A wins B
rps(rock, scissors).
rps(scissors, paper).
rps(paper, rock).

shape_score(rock, 1).
shape_score(paper, 2).
shape_score(scissors, 3).

to_rps(x, rock).
to_rps(y, paper).
to_rps(z, scissors).

score1((A, B), Score) :-
    to_rps(B, B1),
    shape_score(B1, S),
    ( rps(B1, A), Score is 6 + S
    ; A = B1, Score is 3 + S
    ; rps(A, B1), Score is S
    ).

score2((A, B), Score) :-
    (B = x, rps(W, A), shape_score(W, S), Score is 6 + S
    ;B = y, shape_score(A, S), Score is 3 + S
    ;B = z, rps(A, L), shape_score(L, Score)
    ).

% io

file_lines(File, Lines) :-
    setup_call_cleanup(open(File, read, In),
        stream_lines(In, Lines),
        close(In)).

stream_lines(In, Lines) :-
    read_string(In, _, Str),
    split_string(Str, "\n", "", Lines).

% parsing

shape1(rock) --> "A".
shape1(paper) --> "B".
shape1(scissors) --> "C".

shape2(x) --> "X".
shape2(y) --> "Y".
shape2(z) --> "Z".

pair(A, B) --> shape1(A), " ", shape2(B).

solve(Fun, Data, Score) :-
    maplist(Fun, Data, Result),
    foldl(plus, Result, 0, Score).

parse(S, Out) :-
    phrase(pair(A, B), S, []),
    Out = (A, B).

main(Lines) :-
    file_lines("input/d02", Lines),
    maplist(atom_codes, Lines, Codes),
    maplist(parse, Codes, Data),
    solve(score1, Data, Score),
    writeln(Score).
