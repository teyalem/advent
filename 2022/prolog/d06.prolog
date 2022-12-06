load(Data) :-
    File = "../input/d06",
    setup_call_cleanup(open(File, read, In),
        read_string(In, _, Str),
        close(In)),
    atom_codes(Str, Data).

all_diff(Xs) :-
    list_to_set(Xs, Set),
    length(Xs, L), length(Set, L).

find_start(N, I, [First|Packet], Start) :-
    length(H, N),
    append(H, _, [First|Packet]),
    ( all_diff(H) -> Start is I + N
    ; I1 is I + 1, find_start(N, I1, Packet, Start)
    ).

find_start(N, Packet, Start) :-
    find_start(N, 0, Packet, Start).

solve(N, Packet) :-
    find_start(N, Packet, Start),
    writeln(Start).

main :-
    load(Data),
    solve(4, Data),
    solve(14, Data).
