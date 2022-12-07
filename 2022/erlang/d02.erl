-module(d02).
-export([load/0, main/0]).

load() ->
    {ok, F} = file:open("../input/d02", [read]),
    Data = load(F, []),
    file:close(F),
    Data.

load(F, Acc) ->
    case io:fread(F, "", "~s ~s") of
        {ok, [A, B]} -> load(F, [{A, B} | Acc]);
        eof -> lists:reverse(Acc)
    end.

loses_to(rock) -> scissors;
loses_to(paper) -> rock;
loses_to(scissors) -> paper.

wins_to(rock) -> paper;
wins_to(paper) -> scissors;
wins_to(scissors) -> rock.

shape_score(rock) -> 1;
shape_score(paper) -> 2;
shape_score(scissors) -> 3.

to_rps("A") -> rock;
to_rps("B") -> paper;
to_rps("C") -> scissors;
to_rps("X") -> rock;
to_rps("Y") -> paper;
to_rps("Z") -> scissors.

parse1({A, B}) ->
    {to_rps(A), to_rps(B)}.

score1(A, B) ->
    S = shape_score(B),
    L = loses_to(B),
    if
        A == L -> 6 + S;
        B == A -> 3 + S;
        true -> S
    end.

parse2({A, B}) ->
    {to_rps(A), B}.

score2(A, B) ->
    case B of
        "X" -> shape_score(loses_to(A));
        "Y" -> 3 + shape_score(A);
        "Z" -> 6 + shape_score(wins_to(A))
    end.

solve(ProcessF, ScoreF, Data) ->
    L = lists:map(ProcessF, Data),
    L2 = lists:map(fun ({A, B}) -> ScoreF(A, B) end, L),
    io:fwrite("~w~n", [lists:sum(L2)]).

main() ->
    Data = load(),
    solve(fun parse1/1, fun score1/2, Data),
    solve(fun parse2/1, fun score2/2, Data).
