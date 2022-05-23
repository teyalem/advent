program dummy;

uses math;

function is_prime(n : longint) : boolean;
var i : longint;
begin
    is_prime := true;
    for i := 2 to math.floor(sqrt(real(n))) do begin
        if n mod i = 0 then begin
            is_prime := false;
            break;
        end;
    end;
end;

var
    b, h : longint;
    (* d, e, f : integer; *)
    n, i: longint;
begin
    h := 0;
    b := 81 * 100 + 100000;

    (*
    c := b + 17000;
    while true do begin
        f := 1;
        for d := 2 to b do begin
            for e := 2 to b do begin
                if d * e = b then f := 0;
            end;
        end;
        if f = 0 then h := h + 1;
        if b = c then break;
        b := b + 17;
    end;
    *)

    for i := 0 to 1000 do begin
        n := b + 17 * i;
        if not is_prime(n) then inc(h, 1);
    end;

    writeln(h);
end.
