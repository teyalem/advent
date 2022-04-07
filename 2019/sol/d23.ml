open Ut

module Packet = struct
  type t = { addr: int;
             x: int;
             y: int; }
           
  let make addr x y = { addr = addr;
                        x = x;
                        y = y; }

  let addr p = p.addr
  let y p = p.y

  let print p =
    Printf.printf "packet: to %d (%d, %d)\n" p.addr p.x p.y

  let get_packet m =
    if IntCode.is_output_empty m
    then None
    else
      let addr = IntCode.get_output m in
      let x = IntCode.get_output m in
      let y = IntCode.get_output m in
      Some (make addr x y)

  let rec get_packets m =
    let p = get_packet m in
    match p with
      None -> []
    | Some p -> p :: get_packets m

  let send p m =
    let open IntCode in
    set_input m p.x;
    set_input m p.y;

end

module Network = struct
  type t = { machines: IntCode.t array;
             queues: Packet.t Queue.t array;
             mutable nat: Packet.t }

  let make ms = { machines = ms;
                  queues = Array.(init (length ms)) (fun _ -> Queue.create ());
                  nat = Packet.make 255 0 ~-1; }

  let init code =
    Array.init 50 (fun i ->
        let m = IntCode.load code in
        IntCode.set_input m i;
        m)
    |> make

  let collect_packets net =
    net.machines
    |> Array.to_list
    |> List.concat_map Packet.get_packets
    |> List.iter (fun p ->
        let addr = Packet.addr p in
        if addr = 255
        then net.nat <- p
        else Queue.push p net.queues.(addr))

  let send_packet net p i =
    Packet.send p net.machines.(i)

  let send_packets net =
    let idle = ref true in
    for i = 0 to Array.length net.machines - 1 do
      let q = net.queues.(i) in
      if Queue.is_empty q
      then IntCode.set_input net.machines.(i) ~-1
      else begin
        idle := false;
        Queue.iter (fun p ->
            let addr = Packet.addr p in
            send_packet net p addr)
          q;
        Queue.clear q
      end
    done;
    !idle

  let run net =
    Array.iter IntCode.run net.machines;
    collect_packets net;
    send_packets net

  let rec run_part1 net = 
    ignore @@ run net;
    if Packet.y net.nat = -1
    then run_part1 net
    else Packet.y net.nat

  let run_cont net =
    let rec loop ys =
      if run net
      then
        let y = Packet.y net.nat in
        if List.mem y ys
        then y
        else begin
          send_packet net net.nat 0;
          loop (y::ys)
        end
      else loop ys
    in
    loop []

end

let main path =
  let data = open_in path |> IO.read_file |> IntCode.parse_code in
  begin
    (* PART 1 *)
    let net = Network.init data in
    Network.run_part1 net |> print_int;

    print_newline ();

    (* PART 2 *)
    let net = Network.init data in
    Network.run_cont net |> print_int;

  end

let () = Arg.parse [] main ""
