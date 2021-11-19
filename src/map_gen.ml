open Format
open Printf

exception NotNumber

type state =
  | Flagged
  | Discovered of bool

type tile =
  | Bomb of state
  | Numtile of state * int

type config = {
  cols : int;
  rows : int;
  bombs : int;
}

(* type grid = tile array array *)

(* DONT TOUCH HERE START --WORKING ON IT *)

type t = tile array array
(* Creates a start grid with only Numtile that are not discovered and
   contain value 0 *)

let get_tile (grid : t) x y = grid.(x).(y)

let set_tile (grid : t) x y item = grid.(x).(y) <- item

let num_rows (grid : t) = Array.length grid

(** [reveal_tile tile] reveals a tile (e.g. false -> true) *)
let reveal_tile (tile : tile) : tile =
  match tile with
  | Bomb (Discovered false) -> Bomb (Discovered true)
  | Numtile (Discovered false, num) -> Numtile (Discovered true, num)
  | _ -> tile

(** [reveal_bomb tile] reveals a bomb tile (e.g. false -> true) *)
let reveal_bomb (tile : tile) : tile =
  if tile = Bomb (Discovered false) then Bomb (Discovered true)
  else tile

(** [reveal_bombs grid] reveals all bombs in grid. *)
let reveal_bombs (grid : t) : t =
  Array.map (fun elem -> Array.map reveal_bomb elem) grid

(** [reveal_all grid] reveals all tiles in grid. *)
let reveal_all (grid : t) : t =
  Array.map (fun elem -> Array.map reveal_tile elem) grid

let num_rows (grid : t) = Array.length grid

let num_cols (grid : t) = Array.length grid.(0)

(* make this return type t instead of tile array array LATER *)
(* we need to create our own version of Array.make_matrix *)
let grid_gen rows cols =
  Array.make_matrix rows cols (Numtile (Discovered false, 0))

(* DONT TOUCH HERE END --WORKING ON IT *)

(* Check to see if neighbors of index are inbound of the grid's
   dimension. *)
let check fig x y = x >= 0 && y >= 0 && x < fig.cols && y < fig.rows

(* checks to see if index is a bomb and if so returns the value of 1 to
   add to surround numtiles else returns 0*)
let is_bomb (grid : t) fig x y =
  if check fig x y && grid.(x).(y) = Bomb (Discovered false) then 1
  else 0

(* Randomly distrubutes bomb throughout the grid *)
let place_bombs fig (grid : t) =
  let bombs = ref fig.bombs in
  while bombs <> ref 0 do
    let x = Random.int fig.rows in
    let y = Random.int fig.cols in
    if grid.(x).(y) <> Bomb (Discovered false) then (
      bombs := !bombs - 1;
      grid.(x).(y) <- Bomb (Discovered false))
  done;
  grid

(* Updates the grids NumTiles to the correct int value based off
   nerighboring bombs *)
let give_num x y grid fig =
  if grid.(x).(y) = Numtile (Discovered false, 0) then
    grid.(x).(y) <-
      Numtile
        ( Discovered false,
          is_bomb grid fig (x + 1) (y + 1)
          + is_bomb grid fig (x + 1) y
          + is_bomb grid fig (x + 1) (y - 1)
          + is_bomb grid fig x (y + 1)
          + is_bomb grid fig x y
          + is_bomb grid fig x (y - 1)
          + is_bomb grid fig (x - 1) (y + 1)
          + is_bomb grid fig (x - 1) y
          + is_bomb grid fig (x - 1) (y - 1) )

(* Generates map using give_num *)

let generate_map (fig : config) =
  let grid = grid_gen fig.rows fig.cols |> place_bombs fig in
  for x = 0 to fig.rows - 1 do
    for y = 0 to fig.cols - 1 do
      give_num x y grid fig
    done
  done;
  grid

type cur_gamestate = {
  lives : int;
  cur_bombs : int;
}

(**[num_bombs fig] is the number of bombs in the game*)
let num_bombs (fig : config) : int = fig.bombs

(**[num_flags game_map fig] is the number of flags available in the game*)
let num_flags (game_map : t) (fig : config) =
  let flags = ref 0 in
  for i = 0 to fig.cols - 1 do
    for j = 0 to fig.rows - 1 do
      let r = Array.get game_map.(i) j in
      match r with
      | Bomb Flagged -> incr flags
      | Numtile (Flagged, _) -> incr flags
      | _ -> ()
    done
  done;
  !flags

(**[num_lives game_map fig] is the current game point based on these
   calculations: if a payer hit a bomb their points is deducted by 5 if
   a player flags a tile their points is deducted by 2 any other move
   the player makes their points are deducted by 1*)

let num_lives (game_map : t) (fig : config) =
  let start_lives = ref 3 in
  for i = 0 to fig.cols - 1 do
    for j = 0 to fig.rows - 1 do
      let r = Array.get game_map.(i) j in
      match r with
      | Bomb (Discovered true) -> decr start_lives
      | _ -> ()
    done
  done;!start_lives
(**[dune_state game_map fig] is the current state of the game*)
let game_state (game_map : t) (fig : config) : cur_gamestate =
  let cur_flags = num_flags game_map fig in
  let rem_bombs = num_bombs fig - cur_flags in
  let cur_lives = num_lives game_map fig in
  { lives = cur_lives; cur_bombs = rem_bombs }

(*Add printing numbers for rows and columns above and below*)
let print_numtile = function
  | Numtile (Discovered true, 0) -> printf "🟧  "
  | Numtile (Discovered true, 1) -> printf "1️⃣  "
  | Numtile (Discovered true, 2) -> printf "2️⃣  "
  | Numtile (Discovered true, 3) -> printf "3️⃣  "
  | Numtile (Discovered true, 4) -> printf "4️⃣  "
  | Numtile (Discovered true, 5) -> printf "5️⃣  "
  | Numtile (Discovered true, 6) -> printf "6️⃣  "
  | Numtile (Discovered true, 7) -> printf "7️⃣  "
  | Numtile (Discovered true, 8) -> printf "8️⃣  "
  | _ -> raise NotNumber

(*COME BACK AND FIX PRNTING NUMBERS AT END OF LINE*)
let print_tile tile (fig : config) =
  match tile with
  | Bomb (Discovered false) -> print_string "⬜ "
  | Bomb (Discovered true) -> print_string "🐫 "
  | Bomb Flagged -> print_string "🚩 "
  | Numtile (Discovered false, num) -> print_string "⬜ "
  | Numtile (Discovered true, num) -> print_numtile tile
  | Numtile (Flagged, num) -> print_string "🚩 "

let print_grid (fig : config) (grid : t) =
  print_string "Lives:";
  let cur_lives = (game_state grid fig).lives in
  for hearts = 1 to cur_lives do 
  print_string"🤍"
  done;
  print_string "\nCamels:";
let cur_bombs = (game_state grid fig).cur_bombs in 
for hearts = 1 to cur_bombs do 
  print_string"🐫"
done;
print_string "\n";

  for col = 0 to fig.cols - 1 do
    print_string (sprintf " %d " (col + 1))
  done;
  print_string "\n";
  for row = 0 to fig.rows - 1 do
    for col = 0 to fig.cols - 1 do
      print_tile (get_tile grid row col) fig
    done;
    print_string (sprintf "%d\n" (row + 1))
  done

let fig1 = { cols = 3; rows = 3; bombs = 9 } (* ALL MINES *)

let map1 = generate_map fig1

let fig2 = { cols = 11; rows = 11; bombs = 5 }

let map2 = generate_map fig2
