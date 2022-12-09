:- use_module(io).
:- use_module(library(dcg/basics)).

ls --> "$ ls".
cd(Dir) --> "$ cd", whites, string(Dir).
dir(Name) --> "dir", whites, string(Name).
file(Size, Name) --> integer(Size), whites, string(Name).

change_dir(Dir, Cwd, NewCwd) :-
    string_codes(S, Dir),
    ( S = "/" -> NewCwd = []
    ; S = "..", Cwd = [_|Rest] -> NewCwd = Rest
    ; NewCwd = [Dir | Cwd]
    ).

add_file(Dir, File, Tree, NewTree) :-
    ( get_assoc(Dir, Tree, List) ->
        put_assoc(Dir, Tree, [File|List], NewTree)
    ; put_assoc(Dir, Tree, [File], NewTree)
    ).

read_ls(_, Tree, [], [], Tree).
read_ls(Cwd, Tree, [Line|FileList], NewList, NewTree) :-
    ( phrase(dir(Name), Line) ->
        add_file(Cwd, {dir, Name}, Tree, Tree2),
        read_ls(Cwd, Tree2, FileList, NewList, NewTree)
    ; phrase(file(Size, Name), Line) ->
        add_file(Cwd, {file, Size, Name}, Tree, Tree2),
        read_ls(Cwd, Tree2, FileList, NewList, NewTree)
    ; 
        NewTree = Tree,
        NewList = [Line|FileList]
    ).

construct_dirtree(_, [], Tree, Tree).
construct_dirtree(Cwd, [Com|List], Tree, NewTree) :-
    ( phrase(ls, Com) ->
        read_ls(Cwd, Tree, List, NewList, Tree2),
        construct_dirtree(Cwd, NewList, Tree2, NewTree)
    ; phrase(cd(Dir), Com) ->
        change_dir(Dir, Cwd, NewCwd),
        construct_dirtree(NewCwd, List, Tree, NewTree)
    ).

construct_dirtree(List, Tree) :-
    empty_assoc(E),
    construct_dirtree([], List, E, Tree).

du_list(Tree, Path, File, {OldDu, OldDuTree}, {NewDu, NewDuTree}) :-
    ( File = {file, Size, _} ->
        NewDuTree = OldDuTree,
        NewDu is OldDu + Size
    ; File = {dir, Name} ->
        change_dir(Name, Path, NewPath),
        du_dir(Tree, NewPath, OldDuTree, SubDu, NewDuTree),
        NewDu is OldDu + SubDu
    ).

du_dir(Tree, Path, OldDuTree, Du, NewDuTree) :-
    ( get_assoc(Path, Tree, FileList) ->
        foldl(du_list(Tree, Path), FileList, {0, OldDuTree}, {Du, DuTree2}),
        put_assoc(Path, DuTree2, Du, NewDuTree)
    ).

du_all(Tree, RootDu, DuTree) :-
    empty_assoc(E),
    du_dir(Tree, [], E, RootDu, DuTree).

load(Data) :-
    file_lines("../input/d07", Lines),
    maplist(atom_codes, Lines, Data).

solve(Data) :-
    construct_dirtree(Data, Tree),
    du_all(Tree, RootDu, DuTree),
    assoc_to_values(DuTree, DuList),
    include([X]>>(X =< 100000), DuList, L),
    foldl(plus, L, 0, N),
    writeln(N),
    DiskSpace is 70000000,
    UpdateSize is 30000000,
    SpaceNeeded is UpdateSize + RootDu - DiskSpace,
    include([X]>>(X >= SpaceNeeded), DuList, L2),
    min_member(M, L2),
    writeln(M).

main :-
    load(Data),
    solve(Data).
