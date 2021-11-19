open Printf
open Dunesweeper.Command
open Dunesweeper.Map_gen
open Dunesweeper.Basic_logic

let rules () =
  ANSITerminal.print_string
    ANSITerminal.[ red; Bold ]
    "RULES! \n\
    \ - \"Start\" if you wish to play a default 5x5 grid with 5 bombs \n\n\
    \ Enter \"Create r:int c:int b:int\" which creates a r by c grid \
     with b bombs placed Require: b < r times c \n\n\
    \ Enter \"Rules\" for the rules of Dunesweeper\n"

let intro_information () =
  ANSITerminal.print_string
    ANSITerminal.[ yellow ]
    "How to Begin? \n\
    \ Enter \"Start\" if you wish to play a default 5x5 grid with 5 \
     bombs \n\n\
    \ Enter \"Create r:int c:int b:int\" which creates a r by c grid \
     with b bombs placed Require: b < r times c \n\n\
    \ Enter \"Rules\" for the rules of Dunesweeper\n"

let create_grid line_read =
  match line_read with
  | Start ->
      let fig = { rows = 5; cols = 5; bombs = 5 } in
      generate_map fig
  | Create fig -> generate_map fig
  | _ -> raise Malformed

let quit () =
  ANSITerminal.print_string
    ANSITerminal.[ red ]
    "\n\
     Thank you for playing Dunesweeper Hopefully the 🐫 CAMELS 🐫 did \
     not scare you to badly, and we hope you will be able to get some \
     sleep tonight just remember.... JANE STREET always have evil \
     apocalyptic camels watching you somewhere 👀👀\n"

let rec play_game fig grid =
  (* ANSITerminal.erase Above; *)
  print_grid fig grid;
  if game_over grid fig then begin
    ANSITerminal.print_string
      ANSITerminal.[ green ]
      "Moves: \n\
      \ - Shovel x y : Discover a tile IF it is already undiscoverd\n\
       - Flag x y : Flags/unflags a tile at spot x y\n\
       - Autofill x y : Auto Discovers area around coordinate x y if \
       tile is a numbered tile and sufficent flags are placed\n\
       - Reset: restarts game with identical configuration but \
       different grid\n\
       - Quit: Exits out of game\n";
    print_string "\n> ";
    let move = read_line () |> parse in
    match move with
    | Shovel (x, y) ->
        shovel (x - 1) (y - 1) grid;
        play_game fig grid
    | Flag (x, y) ->
        flag (x - 1) (y - 1) grid;
        play_game fig grid
    | Autofill (x,y) ->
      autofill (x - 1) (y - 1) grid; 
      play_game fig grid
    | Rules ->
        rules ();
        play_game fig grid
    | Reset -> play_game fig (create_grid (Create fig))
    | Quit -> quit ()
    | Error ->
        ANSITerminal.print_string
          ANSITerminal.[ red ]
          "\nINVALID INPUT!!! \nPlease Try again\n";
        play_game fig grid
    | _ ->
        ANSITerminal.print_string
          ANSITerminal.[ red ]
          "\nThose commands can not be used here please try again\n";
        play_game fig grid
  end

(** [main ()] prompts for the game to play, then starts it. *)
let rec begin_game () =
  intro_information ();
  print_string "\n> ";

  match read_line () |> parse with
  | exception End_of_file -> ()
  | Start ->
      let basicfig = { rows = 5; cols = 5; bombs = 5 } in
      play_game basicfig (create_grid Start)
  | Create fig -> play_game fig (create_grid (Create fig))
  | Quit -> quit ()
  | _ ->
      ANSITerminal.print_string
        ANSITerminal.[ red ]
        "\nInvalid Input Please try again\n";
      begin_game ()

let main () =
  ANSITerminal.resize 110 20;
  ANSITerminal.print_string
    ANSITerminal.[ red; Bold; Blink ]
    "\n\nWelcome to Dunesweeper!\n";

  ANSITerminal.print_string
    ANSITerminal.[ white ]
    "In this game there are evil apocalyptic 🐫 CAMELS 🐫 that are here \
     to take over the world,\n\
     your mission, IF YOU CHOOSE TO ACCEPT, is to avoid the camels at \
     all cost and make it out alive!\n";

  ANSITerminal.print_string
    ANSITerminal.[ yellow ]
    "Please enter your name so the 🐫 CAMELS 🐫 will know there future \
     victims";

  print_string "\n> ";

  match read_line () with
  | exception End_of_file -> ()
  | line -> begin_game ()

(* Execute the game engine. *)
let () = main ()
