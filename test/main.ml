open OUnit2
open Dunesweeper.Map_gen
open Dunesweeper.Basic_logic
open Dunesweeper.Display
open Format
open Printf

(* changed Array.get -> get_tile since we're using t not array let _ =
   get_tile map1 0 0;; *)

let pp_tile tile fig =
  match tile with
  | Bomb (Discovered false) -> print_string "(-) "
  | Bomb (Discovered true) -> print_string "(*) "
  | Bomb Flagged -> print_string "(?) "
  | Numtile (Discovered false, num) -> print_string "(-) "
  | Numtile (Discovered true, num) ->
      print_string ("(" ^ string_of_int num ^ ") ")
  | Numtile (Flagged, num) -> print_string "(?) "

let pp_grid (fig : config) (grid : t) =
  for col = 0 to fig.cols - 1 do
    print_string (sprintf " %d  " (col + 1))
  done;
  print_string "\n";
  for row = 0 to fig.rows - 1 do
    for col = 0 to fig.cols - 1 do
      pp_tile (get_tile grid row col) fig
    done;
    print_string (sprintf "%d\n" (row + 1))
  done;
  grid

(* let get_tile_test (name : string) (grid : t) (row : int) (col : int)
   (expected_output : tile) : test = name >:: fun _ -> assert_equal
   expected_output (get_tile grid col row) ~printer: print_tile *)

(* Prints "Success" if items are equal else prints "Failure" *)
let verdict result intended_result =
  if result = intended_result then print_endline "Items are equal."
  else print_endline "Items are not equal."

(* Returns a string with str duplicated num times *)
let rec repeat_str num str =
  if num = 0 then "" else str ^ repeat_str (num - 1) str

(* Shows the state of the current grid in the console *)
let show (config : config) (grid : t) =
  print_endline (repeat_str ((config.cols * 3) + 20) "-");
  print_grid config grid

let show_basic (config : config) (grid : t) =
  print_endline (repeat_str ((config.cols * 3) + 20) "-");
  pp_grid config grid

(* Mines the tile at the specified coordinate *)
let m (x : int) (y : int) (config : config) (grid : t) =
  print_endline
    ("Used [Shovel] on (" ^ string_of_int x ^ "," ^ string_of_int y
   ^ ")");
  shovel x y grid;
  show config grid

(* Flags the tile at the specified coordinate *)
let f (x : int) (y : int) (config : config) (grid : t) =
  flag x y grid;
  print_endline
    ("Used [Flag] on (" ^ string_of_int x ^ "," ^ string_of_int y ^ ")");
  show config grid

(* ----------------- Prototype Shovel And Flag Tests
   Start--------------------- *)
(* Mine entire map *)
let c1 = { cols = 3; rows = 3; bombs = 9 }

let g1 = generate_map c1;;

print_endline "Map 1 Initialized: ";;

show c1 g1;;

m 0 0 c1 g1;;

m 0 1 c1 g1;;

m 0 2 c1 g1;;

m 1 0 c1 g1;;

m 1 1 c1 g1;;

m 1 2 c1 g1;;

m 2 0 c1 g1;;

m 2 1 c1 g1;;

m 2 2 c1 g1

(* Flag toggling *)
let c2 = { cols = 3; rows = 3; bombs = 9 }

let g2 = generate_map c1;;

print_endline "Map 2 Initialized: ";;

show c2 g2;;

f 0 0 c2 g2;;

f 0 1 c2 g2;;

f 0 1 c2 g2;;

f 0 0 c2 g2

(* ----------------- Prototype Shovel And Flag Tests
   End--------------------- *)

let valid_coordinate_printer (value : bool) =
  if value then "Valid Coordinate." else "Invalid Coordinate."

let valid_coordinate_test
    (name : string)
    (grid : t)
    (row : int)
    (col : int)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (valid_coordinate row col grid)
    ~printer:valid_coordinate_printer

let c1' = { cols = 3; rows = 3; bombs = 9 }

let g1' = generate_map c1'

(**Test for map_gen.mli*)
let valid_coordinate_tests =
  [
    valid_coordinate_test "0,0 is a valid coordinate" g1' 0 0 true;
    valid_coordinate_test "0,1 is a valid coordinate" g1' 0 1 true;
    valid_coordinate_test "0,2 is a valid coordinate" g1' 0 2 true;
    valid_coordinate_test "1,3 is a valid coordinate" g1' 1 0 true;
    valid_coordinate_test "1,4 is a valid coordinate" g1' 1 1 true;
    valid_coordinate_test "1,0 is a valid coordinate" g1' 1 2 true;
    valid_coordinate_test "2,0 is a valid coordinate" g1' 2 0 true;
    valid_coordinate_test "2,1 is a valid coordinate" g1' 2 1 true;
    valid_coordinate_test "2,2 is a valid coordinate" g1' 2 2 true;
    valid_coordinate_test "0,3 is an invalid coordinate" g1' 0 3 false;
    valid_coordinate_test "1,5 is an invalid coordinate" g1' 1 5 false;
    valid_coordinate_test "0,-1 is an invalid coordinate" g1' 0 (-1)
      false;
    valid_coordinate_test "-5,2 is an invalid coordinate" g1' (-5) 2
      false;
    valid_coordinate_test "3,0 is an invalid coordinate" g1' 3 0 false;
    valid_coordinate_test "3,3 is an invalid coordinate" g1' 3 3 false;
  ]

let place_bombs_test
    (name : string)
    (fig : config)
    (grid : t)
    (expected_output : t) : test =
  name >:: fun _ -> assert_equal expected_output (place_bombs fig grid)

let give_num_test
    (name : string)
    (x : int)
    (y : int)
    (grid : t)
    (fig : config)
    (expected_output : t) : test =
  give_num x y grid fig;
  name >:: fun _ -> assert_equal expected_output grid

let generate_map_test
    (name : string)
    (fig : config)
    (expected_output : t) : test =
  name >:: fun _ -> assert_equal expected_output (generate_map fig)

let get_tile_test
    (name : string)
    (grid : t)
    (row : int)
    (col : int)
    (expected_output : tile) : test =
  name >:: fun _ -> assert_equal expected_output (get_tile grid col row)

let set_tile_test
    (name : string)
    (grid : t)
    (row : int)
    (col : int)
    (item : tile)
    (expected_output : tile) : test =
  set_tile grid row col item;
  name >:: fun _ -> assert_equal expected_output item

let num_rows_test
    (name : string)
    (grid : t)
    (expected_output : int)
    test =
  name >:: fun _ -> assert_equal expected_output (num_rows grid)

let num_col_test (name : string) (grid : t) (expected_output : int) test
    =
  name >:: fun _ -> assert_equal expected_output (num_cols grid)
(*End of map_gen test*)

let cmp_same_grid grid1 grid2 = grid1 = grid1

let tile_grid_gen_helper tile = tile = Numtile (Discovered false, 0)

let grid_gen_helper grid rows cols =
  let temp = ref true in
  for i = 0 to cols - 1 do
    for j = 0 to rows - 1 do
      temp := !temp && tile_grid_gen_helper (get_tile grid i j)
    done
  done;
  !temp

let grid_gen_test (name : string) (row : int) (col : int) : test =
  name >:: fun _ ->
  if num_rows (grid_gen row col) = row then
    assert (grid_gen_helper (grid_gen row col) row col)
  else assert false

let grid_gen_tests = [ grid_gen_test "true is grid_gen 2 2" 2 2 ]

(* --------------- ALL DEBUGGING TESTS START HERE ------------------- *)

let q1 = { cols = 3; rows = 3; bombs = 4 }

let g11 = generate_map q1;;

print_string "\n\n";;

print_endline "Testing reveal_all: "

let new_grid = reveal_all g11;;

show q1 new_grid

(* --------------- ALL DEBUGGING TESTS END HERE --------------------- *)

let tests =
  "Test Suite for Dunesweeper"
  >::: List.flatten [ valid_coordinate_tests; grid_gen_tests ]

let _ = run_test_tt_main tests

let _ =
  run_test_tt_main tests;

print_endline "water"

let c3 = { cols = 3; rows = 3; bombs = 9 }

let g3 = generate_map c3;;

print_grid c3 g3;;
let neighbors = get_neighbors 1 1 g3;;
print_tile_array neighbors;;
let flags = string_of_int (num_flags_in_list neighbors);;

f 0 0 c3 g3;;

let neighbors = get_neighbors 1 1 g3;;
print_tile_array neighbors;;
let flags = string_of_int (num_flags_in_list neighbors);;

print_endline flags;;

shovel_all 1 1 g3;;
print_grid c3 g3;;