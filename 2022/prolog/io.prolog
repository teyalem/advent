:- module(io, [file_lines/2]).
% io

file_lines(File, Lines) :-
    setup_call_cleanup(open(File, read, In),
        stream_lines(In, Lines),
        close(In)).

stream_lines(In, Lines) :-
    read_string(In, _, Str),
    split_string(Str, "\n", "\n", Lines).

