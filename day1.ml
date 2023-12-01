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

let sum_l (l) = List.fold_left (fun x y -> x + y) 0 l

let search_first (x) = 
  let pos = Str.search_forward (Str.regexp "[0-9]") x 0 in
  String.sub x pos 1 

let search_last (x) =
  let pos = Str.search_backward (Str.regexp "[0-9]") x (String.length x) in
  String.sub x pos 1

let final = read_file "/home/hrisc/Documents/aoc/inputs/day1"
              |> List.map (fun x -> int_of_string (String.concat "" [search_first x ; search_last x]))
              |> sum_l
      
let () = print_int final; print_endline "";;

