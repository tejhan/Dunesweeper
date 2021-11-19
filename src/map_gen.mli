type t
(** The abstract type of values representing grid maps. *)

type state =
  | Flagged
  | Discovered of bool  (** The type of tile state *)

type tile =
  | Bomb of state
  | Numtile of state * int  (** The type of grid tiles *)

type config = {
  cols : int;
  rows : int;
  bombs : int;
}
(** The type of initial game configuration *)

val get_tile : t -> int -> int -> tile
(** [get_tile] returns the tile at the corresponding coordinates. **)

val set_tile : t -> int -> int -> tile -> unit
(** [set_tile] replaces the tile at the corresponding coordinate to the
    given tile. **)

val reveal_tile : tile -> tile
(** [reveal_tile tile] reveals a tile (e.g. false -> true) *)

val reveal_bomb : tile -> tile
(** [reveal_bomb tile] reveals a bomb tile (e.g. false -> true) *)

val reveal_bombs : t -> t
(** [reveal_bombs grid] reveals all bombs in grid. *)

val reveal_all : t -> t
(** [reveal_all grid] reveals all tiles in grid. *)

val num_rows : t -> int
(** [set_tile] returns the number of rows in the given grid. **)

val num_cols : t -> int
(** [set_tile] returns the number of columns in the given grid. **)

val grid_gen : int -> int -> t
(** [grid_gen rows cols] creates a matrix filled with undiscovered
    Numtiles that contain value 0. *)

val place_bombs : config -> t -> t
(** [place_bombs fig grid] randomly distrubutes bomb throughout the
    array. *)

val give_num : int -> int -> t -> config -> unit
(** [give_num x y grid fig] updates the arrays NumTiles to the correct
    int value based off nerighboring bombs. *)

val generate_map : config -> t
(** [generate_map fig grid] generates map using give_num. *)

val print_tile : tile -> config -> unit

val print_grid : config -> t -> unit

val num_lives : t -> config -> int
