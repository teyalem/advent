open Ut

let passcode = "vwbaicqe"

let dirs = [
  "U", (-1,  0);
  "D", ( 1,  0);
  "L", ( 0, -1);
  "R", ( 0,  1);
] |> List.to_seq

let is_open c = 'b' <= c && c <= 'f'

let open_doors code =
  Digest.(string code |> to_hex)
  |> String.to_seq
  |> Seq.take 4
  |> Seq.zip dirs
  |> Seq.filter (fun (_, c) -> is_open c)
  |> Seq.map fst
  |> List.of_seq

module Vault = struct
  type space = string
  type state = (int * int) * string
  type data = string
  type weight = int

  let data_id = ""

  let is_end _ (((x, y), _), _) =
    x = 3 && y = 3

  let neighbors code (((x, y), _), d) =
    open_doors (code ^ d)
    |> List.map (fun (i, (dx, dy)) -> 1, ((x+dx, y+dy), d ^i), d ^ i)
    |> List.filter (fun (_, ((x, y), _), _) -> 0 <= x && x < 4 && 0 <= y && y < 4)
end

module Vault2 = struct
  include Vault

  let neighbors code sd = 
    neighbors code sd |> List.map (fun (_, s, d) -> s, d)
end

module S = struct
  type t = (int * int) * string
  let compare = Stdlib.compare
end

let shortest code =
  Pathfind.dijkstra (module S) (module Int) (module Vault)
    ~start: ((0, 0), "") code

let find_pathes code =
  Pathfind.bfs_collect (module S) (module Vault2)
    ~start: ((0, 0), "") code

let () =
  begin
    (* PART 1 *)
    passcode
    |> shortest
    |> snd
    |> (Printf.printf "%s\n");

    (* PART 2 *)
    passcode
    |> find_pathes
    |> List.fold_left (fun p n -> if String.(length p >= length n) then p else n) ""
    |> String.length
    |> Printf.printf "%d\n";
  end
