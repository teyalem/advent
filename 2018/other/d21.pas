program test;

type res = array[1..100000] of int64;

function mem(arr : res; len : int64; n : int64) : boolean;
var i : int64;
begin
    mem := false;
    for i := 1 to len do begin
        if arr[i] = n then exit(true);
    end;
end;

var
    r2, r3 : int64;
    i : int64;
    ns : res;
    found : boolean;
begin
    r3 := 0;
    for i := 1 to 100000 do ns[i] := 0;
    i := 0;

    found := false;
    while not found do begin
        r2 := r3 or 65536;
        r3 := 7637914;

        repeat
            r3 := (r3 + r2 and 255) and 16777215;
            r3 := (r3 * 65899) and 16777215;

            r2 := r2 div 256;
        until r2 < 1;

        if mem(ns, i, r3) then begin
            found := true;
        end
        else begin
            inc(i);
            ns[i] := r3;
        end;
    end;

    writeln(ns[1]);
    writeln(ns[i]);
end.
