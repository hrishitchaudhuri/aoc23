(* INPUT: <filename> *)

let read_file filename = 
  let lines = ref [] in
    let chan = open_in filename in
      try
        while true; do
          lines := input_line chan :: !lines
        done; !lines
      with End_of_file ->
        close_in chan;
        List.rev !lines

let rec get_max (l) = match l with 
  | h :: t -> if (h > get_max t) then h else get_max t
  | [] -> 0

let process_str line =
  let _ = Str.search_forward (Str.regexp {|[0-9]+|}) line 0 in 
    let day = int_of_string (Str.matched_string line) in
      let rec get_all_matches regexp i = match Str.search_forward (Str.regexp ({|[0-9]+ |} ^ regexp)) line i with
        | i -> let s = Str.matched_string line in
                  if (s.[1] == ' ') 
                    then int_of_string (String.sub s 0 1) :: get_all_matches regexp (i + 1)
                  else int_of_string (String.sub s 0 2) :: get_all_matches regexp (i + 1) 
        | exception Not_found -> []
      in
        let process colour = get_all_matches colour 0 (*|> replace_list line *)|> get_max in
          let colours = (process "blue", process "red", process "green")
    in (day, colours)

(* 
let check_valid colour_tup =
  let (day, (blu, red, grn)) = colour_tup in
    if (blu <= 14) && (red <= 12) && (grn <= 13) 
      then day
    else 0
*)

let check_valid colour_tup =
  let (day, (blu, red, grn)) = colour_tup in
    blu * red * grn

let rec sum_ds = function
  | [] -> 0
  | h :: t -> check_valid h + sum_ds t

let final file = read_file file 
                    |> List.map process_str
                    |> sum_ds


let () = read_line() |> final |> print_int