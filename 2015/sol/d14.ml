open Ut

let calc_distance total_sec speed fly_sec rest_sec =
  let round_sec = fly_sec + rest_sec in
  let nr = total_sec / round_sec
  and last_fly_sec = min fly_sec (total_sec mod round_sec) in
  speed * (fly_sec*nr + last_fly_sec)

type state = Flying | Resting

let speed (sp, _, _) = sp
let flying_sec (_, fs, _) = fs
let resting_sec (_, _, rs) = rs

let calc_scores tsec deers =
  let deers = Array.of_list deers in
  let len = Array.length deers in
  let dstates = Array.init len (fun i -> Flying, flying_sec deers.(i))
  and ddists = Array.make len 0
  and dscores = Array.make len 0 in

  let update_state n (s, i) =
    match s with
    | Flying -> if i <= 1 then Resting, resting_sec deers.(n) else Flying, i-1
    | Resting -> if i <= 1 then Flying, flying_sec deers.(n) else Resting, i-1

  and update_distances () =
    Array.iteri (fun i (s, _) ->
        match s with
        | Flying -> ddists.(i) <- ddists.(i) + speed deers.(i)
        | Resting -> ())
      dstates

  and update_scores () =
    let maxdist = Array.fold_left max min_int ddists in
    Array.iteri (fun i d ->
        if d = maxdist then dscores.(i) <- dscores.(i) + 1)
      ddists
  in

  for _ = 1 to tsec do
    update_distances ();
    update_scores ();
    Array.iteri (fun i s -> dstates.(i) <- update_state i s) dstates;
  done;
  Array.fold_left max min_int dscores

let parse str =
  Scanf.sscanf str
    "%s can fly %d km/s for %d seconds, but then must rest for %d seconds."
    (fun _name sp fs rs -> sp, fs, rs)

let total_sec = 2503

let () =
  let data = IO.read_lines () |> List.map parse in
  begin
    (* PART 1 *)
    data
    |> List.map (fun (sp, fs, rs) -> calc_distance total_sec sp fs rs)
    |> List.fold_left max min_int
    |> Printf.printf "%d\n";

    (* PART 2 *)
    calc_scores total_sec data
    |> Printf.printf "%d\n";
  end
