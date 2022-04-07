open Ut

module StringSet = Set.Make(String)

module IAList = struct

  let parse_line str =
    let parse_ingredient str =
      Delim.split " " str |> StringSet.of_list
    in

    let parse_allergen str =
      Delim.split ", " str |> StringSet.of_list
    in

    Scanf.sscanf str "%s@(contains %s@)"
      (fun ingred aller -> parse_ingredient ingred, parse_allergen aller)

  let parse_list sl =
    List.map parse_line sl

  (* Each allergen is found in exactly one ingredient. Each ingredient contains
   * zero or one allergen. Allergens aren't always marked; when they're listed
   * (as in (contains nuts, shellfish) after an ingredients list), the
   * ingredient that contains each listed allergen will be somewhere in the
   * corresponding ingredients list. However, even if an allergen isn't listed,
   * the ingredient that contains that allergen could still be present: maybe
   * they forgot to label it, or maybe it was labeled in a language you don't
   * know. *)

  let match_allergen allergen ialist =
    let ingreds =
      ialist |> List.filter_map
        (fun (ig, al) ->
           if StringSet.mem allergen al
           then Some ig
           else None)
    in
    if List.length ingreds = 1
    then List.hd ingreds
    else List.fold_left StringSet.inter (List.hd ingreds) (List.tl ingreds)

  let rec match_allergens ialist = 
    let all_allergens =
      ialist |> List.split |> snd
      |> List.fold_left StringSet.union StringSet.empty
      |> StringSet.elements
    in

    (* find matching allergens *)
    let found_allergens =
      all_allergens
      |> List.filter_map (fun allergen ->
          let possible_ingreds = match_allergen allergen ialist in
          if StringSet.cardinal possible_ingreds = 1 (* only one possiblity *)
          then
            let ingred = StringSet.elements possible_ingreds |> List.hd in
            (* Printf.printf "found: %s %s\n" ingred allergen; (* debug *)*)
            Some (ingred, allergen)
          else None (* multiple possiblity found; do it later *)
        )
    in

    let ialist =
      ialist |> List.map (fun (il, al) ->
          found_allergens
          |> List.fold_left (fun (il, al) (i, a) ->
              StringSet.remove i il,
              StringSet.remove a al) (il, al))
      |> List.filter (fun (il, al) -> not @@ StringSet.(is_empty il || is_empty al))
    in

    found_allergens @
    if List.length ialist = 0
    then []
    else match_allergens ialist

end

let main path =
  let data = open_in path |> IO.input_lines |> IAList.parse_list in
  begin
    (* PART 1*)
    let allergens = IAList.match_allergens data in
    let ingreds = data |> List.map fst in
    let ingred = allergens |> List.map fst in
    ingreds
    |> List.map (fun il ->
        ingred |> List.fold_left (fun il i -> StringSet.remove i il) il
        |> StringSet.elements)
    |> List.concat
    |> List.length
    |> print_int;

    print_newline ();

    (* PART 2*)
    (* collect allergic ingredients *)
    let ingred_list = allergens |> List.sort
                          (fun (_, a) (_, b) -> String.compare a b)
                        |> List.map fst
    in
    String.concat "," ingred_list |> print_string

  end

let _ = Arg.parse [] main ""
