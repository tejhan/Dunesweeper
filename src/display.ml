open Map_gen

(**[reveal_tile tile] reveals a tile (e.g. false -> true) *)
(* let reveal_tile (tile : tile) : tile = match tile with | Bomb
   (Discovered false) -> Bomb (Discovered true) | Numtile (Discovered
   (false), num) -> Numtile (Discovered (true), num) | _ -> tile let
   reveal_all (grid : t) = Array.map (fun elem -> Array.map reveal_tile
   elem) grid *)
(* Array.map (Array.map reveal_tile ) grid

   let fig1 = {cols =3;rows=3;bombs=9} (* ALL MINES *) let map1 =
   generate_map fig1 *)

(* work on reveal (all and just bombs); work on clear; refactor code and
   stuff *)
