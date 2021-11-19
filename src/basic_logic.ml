open Map_gen
open Command

let tile = Bomb (Discovered false)

let tile1 = Numtile (Discovered false, 0)

(* ======================== LOGIC START =========================== *)
(* <Logic> *)
(* Discovers the tile at the given position, if it is undiscovered. *)
let shovel (row : int) (col : int) (map : t) =
  let current = get_tile map row col in
  match current with
  | Bomb (Discovered false) ->
      set_tile map row col (Bomb (Discovered true));
      print_endline "YOU HIT A BOMB"
  | Numtile (Discovered false, num) ->
      set_tile map row col (Numtile (Discovered true, num))
  | _ ->
      ANSITerminal.print_string
        ANSITerminal.[ red ]
        "\nTHIS TILE CANNOT BE SHOVELED PLEASE TRY AGAIN\n"

(* Flags the tile at the given position, if it is undiscovered, removes
   flag if it is already flagged *)
let flag (row : int) (col : int) (map : t) =
  let current = get_tile map row col in
  if
    match current with
    | Bomb (Discovered false) -> true
    | Numtile (Discovered false, num) -> true
    | _ -> false
  then (
    match current with
    | Bomb (Discovered false) -> set_tile map row col (Bomb Flagged)
    | Numtile (Discovered false, num) ->
        set_tile map row col (Numtile (Flagged, num))
    | _ ->
        ();
        print_endline "Flag added!")
  else
    match current with
    | Bomb Flagged -> set_tile map row col (Bomb (Discovered false))
    | Numtile (Flagged, num) ->
        set_tile map row col (Numtile (Discovered false, num))
    | _ ->
        ();
        print_endline "Flag removed!"

(* Returns true if the specified coordinate is valid, false
   otherwise. *)
let valid_coordinate (row : int) (col : int) (matrix : t) =
  if
    row < 0
    || row > num_rows matrix - 1
    || col > num_cols matrix - 1
    || col < 0
  then false
  else true

let is_numtile tile = 
  match tile with 
  | Numtile (state, num) -> true 
  | _ -> false 

let tile_discovered tile =
  match tile with
  | Numtile (state, num) -> state = Discovered true
  | _ -> true

let check_discovered (grid : t) fig =
  let temp = ref true in
  for i = 0 to fig.cols - 1 do
    for j = 0 to fig.rows - 1 do
      temp := !temp && tile_discovered (get_tile grid i j)
    done
  done;
  !temp

let alive grid fig = num_lives grid fig <> 0

let game_over grid fig =
  not (alive grid fig && check_discovered grid fig)

(* let temp = [||] let get_neighbors row col (matrix : tile array array)
   = if (valid_coordinate (row-1) (col-1) matrix) then (Array.append
   temp [|matrix.(row-1).(col-1)|]) else [||] if (valid_coordinate
   (row-1) (col) matrix) then (Array.append temp
   [|matrix.(row-1).(col)|]) else [||] if (valid_coordinate (row-1)
   (col+1) matrix) then (Array.append temp [|matrix.(row-1).(col+1)|])
   else [||] if (valid_coordinate (row) (col-1) matrix) then
   Array.append temp [|matrix.(row).(col-1)|] else [||] if
   (valid_coordinate (row) (col+1) matrix) then Array.append temp
   [|matrix.(row).(col+1)|] else [||] if (valid_coordinate (row+1)
   (col-1) matrix) then Array.append temp [|matrix.(row+1).(col-1)|]
   else [||] if (valid_coordinate (row+1) (col) matrix) then
   Array.append temp [|matrix.(row+1).(col)|] else [||] if
   (valid_coordinate (row+1) (col+1) matrix) then Array.append temp
   [|matrix.(row+1).(col+1)|] else [||] temp *)
let empty = Array.make 0 (Bomb (Discovered false))

let get_neighbors (row : int) (col : int) (matrix : t) =
  let (temp' : tile array ref) = ref empty in
  temp' :=
    if valid_coordinate (row - 1) (col - 1) matrix then
      Array.append !temp' [| get_tile matrix (row - 1) (col - 1) |]
    else Array.append !temp' empty;
  temp' :=
    if valid_coordinate (row - 1) col matrix then
      Array.append !temp' [| get_tile matrix (row - 1) col |]
    else Array.append !temp' empty;
  temp' :=
    if valid_coordinate (row - 1) (col + 1) matrix then
      Array.append !temp' [| get_tile matrix (row - 1) (col + 1) |]
    else Array.append !temp' empty;
  temp' :=
    if valid_coordinate row (col - 1) matrix then
      Array.append !temp' [| get_tile matrix row (col - 1) |]
    else Array.append !temp' empty;
  temp' :=
    if valid_coordinate row col matrix then
      Array.append !temp' [| get_tile matrix row col |]
    else Array.append !temp' empty;
  temp' :=
    if valid_coordinate row (col + 1) matrix then
      Array.append !temp' [| get_tile matrix row (col + 1) |]
    else Array.append !temp' empty;
  temp' :=
    if valid_coordinate (row + 1) (col - 1) matrix then
      Array.append !temp' [| get_tile matrix (row + 1) (col - 1) |]
    else Array.append !temp' empty;
  temp' :=
    if valid_coordinate (row + 1) col matrix then
      Array.append !temp' [| get_tile matrix (row + 1) col |]
    else Array.append !temp' empty;
  temp' :=
    if valid_coordinate (row + 1) (col + 1) matrix then
      Array.append !temp' [| get_tile matrix (row + 1) (col + 1) |]
    else Array.append !temp' empty;
  !temp'

(* Converts a tile to a string *)
let tile_to_string tile =
  match tile with
  | Bomb (Discovered false) -> "Undiscovered Bomb"
  | Bomb (Discovered true) -> "Discovered"
  | Bomb Flagged -> "Flagged Bomb"
  | Numtile (Discovered false, num) ->
      "Undiscovered Numtile of value " ^ string_of_int num
  | Numtile (Discovered true, num) ->
      "Discovered Numtile of value " ^ string_of_int num
  | Numtile (Flagged, num) ->
      "Flagged Numtile of value " ^ string_of_int num

let tile_array_to_string_array (array : tile array) =
  Array.map tile_to_string array

(* Prints an array of tiles into the console *)
let print_tile_array (array : tile array) =
  print_string "[ ";
  for i = 0 to Array.length array - 1 do
    print_string (tile_to_string array.(i));
    if i <> Array.length array - 1 then print_string ", ";
    if (i + 1) mod 3 = 0 && i <> Array.length array - 1 then
      print_endline ""
  done;
  print_string " ]";
  print_endline "";
;;

(* let valid_flag (tile : tile) = 
  match tile with 
  | Bomb Discovered false -> false 
  | Bomb Discovered true -> false 
  | Bomb Flagged -> true
  | Numtile (Discovered (false), num) -> true
  | Numtile (Discovered (true), num) -> true
  | Numtile (Flagged, num) -> false *)

(* let valid_flags ( array : tile array) = 
  Array.for_all valid_flag array *)

let num_flags_in_list (array : tile array) = 
  let count = ref 0 in 
    for i = 0 to ((Array.length array) - 1) do 
      count := (!count) + (match array.(i) with
              | Bomb Flagged -> 1
              | Numtile (Flagged, num) -> 1
              | _ -> 0)
    done;!count

let shovel_all x y grid = 
  if valid_coordinate (x-1) (y-1) grid then shovel (x-1) (y-1) grid;
  if valid_coordinate (x-1) (y) grid then shovel (x-1) (y) grid;
  if valid_coordinate (x-1) (y+1) grid then shovel (x-1) (y+1) grid;
  if valid_coordinate (x) (y-1) grid then shovel (x) (y-1) grid;
  if valid_coordinate (x) (y) grid then shovel (x) (y) grid;
  if valid_coordinate (x) (y+1) grid then shovel (x) (y+1) grid;
  if valid_coordinate (x+1) (y-1) grid then shovel (x+1) (y-1) grid;
  if valid_coordinate (x+1) (y) grid then shovel (x+1) (y) grid;
  if valid_coordinate (x+1) (y+1) grid then shovel (x+1) (y+1) grid;
;;

let autofill (x : int) (y : int) (grid : t) = 
  if (is_numtile (get_tile grid x y)) then
    shovel_all x y grid;

(* </Logic>*)
(* ========================= LOGIC END ============================== *)