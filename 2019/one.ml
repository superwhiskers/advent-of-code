(* advent of code 2019 day 1 implementation in ocaml *)
(* takes mass as space-separated arguments to the program *)

(* accumulates an integer array *)
let rec accumulate_int_array' i arr acc =
        if i >= Array.length arr then acc
        else accumulate_int_array' (i + 1) arr (acc + arr.(i))
;;
let accumulate_int_array arr = accumulate_int_array' 0 arr 0;;

(* calculates fuel requirements for a given mass *)
let calculate_fuel m = (m / 3) - 2;;

(* accumulates fuel requirements for an array of mass *)
let rec accumulate_fuel' i arr acc =
        if i >= Array.length arr then acc
        else accumulate_fuel' (i + 1) arr (acc + calculate_fuel arr.(i))
;;
let accumulate_fuel arr = accumulate_fuel' 0 arr 0;;

(* compounds fuel requirements for a given amount of fuel *)
let rec compound_fuel_mass' last acc =
        let fuel = calculate_fuel last in
        if fuel <= 0 then acc
        else compound_fuel_mass' fuel (acc + fuel)
;;
let compound_fuel_mass m = compound_fuel_mass' m m;;

let () =
        let integer_argv = (Array.map (fun x -> int_of_string x) (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))) in
        let accumulated_and_calculated = (accumulate_fuel integer_argv) in
        let accumulated_and_compounded = (Array.map (fun x -> (compound_fuel_mass (calculate_fuel x))) integer_argv) in
        Printf.printf "accumulated_and_calculated: %d\n" accumulated_and_calculated;
        Printf.printf "accumulated_and_compounded: %d\n" (accumulate_int_array accumulated_and_compounded);
