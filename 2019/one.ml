(* advent of code 2019 day 1 implementation in ocaml *)
(* takes mass as space-separated arguments to the program *)

let calculate_fuel m = (m / 3) - 2;;

let rec accumulate_fuel i arr acc =
        if i >= Array.length arr then acc
        else accumulate_fuel (i + 1) arr (acc + (calculate_fuel arr.(i)))
;;

let () =
        Printf.printf "%d" (accumulate_fuel 0 (Array.map (fun x -> int_of_string x) (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))) 0)
