open Ut

module IntTile = struct
  type t = int
  let default = 0
  let of_char c = Char.(code c - code '0')
  let to_char n = if n = 10 then '*' else Char.(chr @@ n + code '0')
end

module IB = Block.Make(IntTile)

let neighs = [
  -1, -1; 0, -1; 1, -1;
  -1,  0;        1,  0;
  -1,  1; 0,  1; 1,  1 ]

let step b : unit =
  IB.iteri (fun x y e -> IB.set b x y (e+1)) b;
  let tens = Queue.create () in
  IB.iteri (fun x y e -> if e > 9 then Queue.add (x, y) tens) b;
  let flash x y =
    IB.set b x y 0;
    List.map (fun (dx, dy) -> x+dx, y+dy) neighs
    |> List.filter (fun (x, y) ->
        0 <= x && x < IB.dimx b && 0 <= y && y < IB.dimy b)
    |> List.iter (fun (x, y) ->
        let l = IB.get b x y in
        if 0 < l && l < 10 then begin
          IB.set b x y (l+1);
          if l = 9 then Queue.add (x, y) tens
        end)
  in
  let rec aux () =
    if not @@ Queue.is_empty tens then
    let x, y = Queue.take tens in begin flash x y; aux () end
  in
  aux ()

let collect_flash b n =
  let rec aux c i =
    if i = n then c
    else begin
      step b;
      aux (c + IB.count_occur 0 b) (i+1)
    end
  in
  aux 0 0

let find_sync b =
  let dimx = IB.dimx b and dimy = IB.dimy b in
  let rec aux i =
    step b;
    if IB.count_occur 0 b = dimx*dimy
    then i
    else aux (i+1)
  in
  aux 1

let () =
  let data = IO.read_lines () |> IB.parse in
  begin
    (* PART 1 *)
    collect_flash (IB.copy data) 100 |> Printf.printf "%d\n";

    (* PART 2 *)
    find_sync data |> Printf.printf "%d\n";
  end
