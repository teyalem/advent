open Ut

let (>|) a b =
  match a with
  | Some _ -> a
  | None -> b

module Rule = struct
  (* rule type *)
  type t = Letter of char
         | List of int list
         | Or of int list * int list

  (* Associative Rule List *)
  type rules = (int * t) list

  (* debug: print rule number and content, as read *)
  let print (n, rule) =
    let open Printf in
    printf "%d:" n;
    match rule with
    | Letter c -> printf " %c\n" c
    | List l ->
      List.iter (printf " %d") l;
      print_newline ()
    | Or (a, b) -> 
      List.iter (printf " %d") a;
      print_string " |";
      List.iter (printf " %d") b;
      print_newline ()

  (* parse a rule *)
  let parse str =
    let open Scanf in
    (* turn exceptions to option *)
    let try_s f str =
      match f str with
      | a -> Some a
      | exception _ -> None
    in

    let parse_letter =
      try_s (fun str -> sscanf str "\"%c\"" (fun c -> Letter c))

    and parse_list str =
      Delim.split " " str
      |> List.map int_of_string
      |> fun l -> List l

    and parse_or str =
      match Delim.split "|" str with
      | [a; b] ->
        let a = Delim.split " " a |> List.map int_of_string
        and b = Delim.split " " b |> List.map int_of_string
        in Some (Or (a, b))
      | _ -> None
    in

    sscanf str "%d: %s@!" (fun n s ->
        let rule =
          match parse_letter s >| parse_or s with
          | Some a -> a
          | None -> parse_list s
        in
        n, rule)

  (* evaluate rule with sequence
   * true, consumed sequence
   * false, out = in *)
  let rec eval (rule_num: int) (rules: rules) (seq: char Seq.t) : (bool * char Seq.t) list =

    let eval_list (l: int list)  (seq: char Seq.t) : (bool * char Seq.t) list =
      let rec loop tmp_list = function
        | [] -> tmp_list
        | r :: rest -> 
            List.(concat_map (fun (_, s) -> eval r rules s) tmp_list
            |> filter (fun (b, _) -> b)
            |> fun tl -> loop tl rest)
      in
      loop [true, seq] l
    in

    match List.assoc rule_num rules with
    | Letter a -> begin
        match seq () with
        | Seq.Cons (c, seq) when c = a -> [true, seq]
        | _ -> []
      end

    | List l -> eval_list l seq

    | Or (a, b) ->
      let a = eval_list a seq in
      let b = eval_list b seq in
      List.concat [a; b]

  let compile rule_num rules = fun str ->
    let seq = String.to_seq str in
    List.rev (eval rule_num rules seq)
    |> List.find_map (fun (b, s) -> match s () with Seq.Nil -> Some b | _ -> None)
    |> Option.value ~default: false

end

let read_rule_msg file =
  match Delim.split "\n\n" file with
  | [rules; messages] ->
    let rules = Delim.split_line rules
                |> List.map Rule.parse
    and messages = Delim.split_line messages
    in
    rules, messages
  | _ -> assert false


let main path =
  let rules, messages = open_in path |> IO.input_all |> read_rule_msg in
  begin
    (* PART 1 *)
    let rule_0 = Rule.compile 0 rules in
    List.filter rule_0 messages
    |> List.length
    |> print_int;

    print_newline ();

    (* PART 2 *)
    let rule_8 = Rule.parse "8: 42 | 42 8"
    and rule_11 = Rule.parse "11: 42 31 | 42 11 31"
    in

    let rules = rules
                |> List.remove_assoc 8
                |> List.remove_assoc 11
                |> fun l -> [rule_8; rule_11] @ l
    in

    let rule_0 = Rule.compile 0 rules in
    List.filter rule_0 messages
    |> List.length
    |> print_int

  end

let _ = Arg.parse [] main ""
