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

let get_int_str = function
  | "zero" -> "0"
  | "one" -> "1"
  | "two" -> "2"
  | "three" -> "3"
  | "four" -> "4"
  | "five" -> "5"
  | "six" -> "6"
  | "seven" -> "7"
  | "eight" -> "8"
  | "nine" -> "9" 
  | x -> x

let sum_l (l) = List.fold_left (fun x y -> x + y) (int_of_string "0") l

let search_first (x) = 
  let _ = Str.search_forward (Str.regexp {|[0-9]\|one\|two\|three\|four\|five\|six\|seven\|eight\|nine\|zero|}) x (int_of_string "0")  in
  Str.matched_string x 

let search_last (x) =
  let _ = Str.search_backward (Str.regexp {|[0-9]\|one\|two\|three\|four\|five\|six\|seven\|eight\|nine\|zero|}) x (String.length x) in
  Str.matched_string x

let final (file) = read_file file
              |> List.map (fun x -> int_of_string (String.concat "" [ get_int_str (search_first x) ; get_int_str (search_last x)]))
              |> sum_l
      
let () = read_line() |> final |> print_int; print_endline "";;
