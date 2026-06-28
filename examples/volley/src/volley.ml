open Gamelle
open Libvolley

(* Rendering only — no simulation. Used both by the singleplayer loop (on the
   state it just computed) and by the multiplayer client (on the state received
   from the server). *)
let draw_state ~io { player1; player2; ball; points1; points2 } =
  List.iter (Physics.fill ~io ~color:Color.white) world;
  Physics.fill ~io ~color:Color.blue player1.shape;
  Physics.fill ~io ~color:Color.blue player2.shape;
  Physics.fill ~io ~color:Color.red ball;
  List.iter (Physics.draw ~io) world;
  Physics.draw ~io player1.shape;
  Physics.draw ~io player2.shape;
  Physics.draw ~io ball;
  Text.draw ~io ~size:40 ~color:Color.white (string_of_int points1)
    ~at:(Point.v 20.0 10.0);
  Text.draw ~io ~size:40 ~color:Color.white (string_of_int points2)
    ~at:(Point.v 960.0 10.0)

(* Singleplayer: the whole simulation runs locally, two players share the
   keyboard (WASD and the arrow keys). *)
let rec singleplayer ~io state =
  let render_io = View.translate (Vec.v 0.0 500.0) io in
  Box.fill ~io:render_io ~color:Color.black (Window.box ~io:render_io);
  if Input.is_down ~io (`input_char "f") then
    Window.set_fullscreen ~io (not (Window.get_fullscreen ~io));
  if Input.is_pressed ~io `escape then raise Exit;
  let state =
    if Input.is_down ~io (`input_char "r") then initial_state
    else
      let event = Input_event.of_io ~io in
      let input1 =
        read_player_input event ~left:(`physical_char 'a')
          ~right:(`physical_char 'd') ~up:(`physical_char 'w')
          ~down:(`physical_char 's')
      in
      let input2 =
        read_player_input event ~left:`arrow_left ~right:`arrow_right
          ~up:`arrow_up ~down:`arrow_down
      in
      step ~dt:(dt ~io) ~input1 ~input2 state
  in
  draw_state ~io:render_io state;
  next_frame ~io;
  singleplayer ~io state

(* Multiplayer client: the server owns the simulation. Each frame we send our
   own input (WASD, same controls whichever side the server assigns us) and
   render the latest state the server broadcast. *)
let rec multiplayer ~io conn ~me state =
  let render_io = View.translate (Vec.v 0.0 500.0) io in
  if Input.is_down ~io (`input_char "f") then
    Window.set_fullscreen ~io (not (Window.get_fullscreen ~io));
  if Input.is_pressed ~io `escape then raise Exit;
  let event = Input_event.of_io ~io in
  let input =
    read_player_input event ~left:(`physical_char 'a')
      ~right:(`physical_char 'd') ~up:(`physical_char 'w')
      ~down:(`physical_char 's')
  in
  Net.send conn (Yojson.Safe.to_string (player_input_to_yojson input));
  let me, state =
    List.fold_left
      (fun (me, state) msg ->
        match to_client_of_yojson (Yojson.Safe.from_string msg) with
        | Ok (Welcome n) -> (n, state)
        | Ok (State st) -> (me, st)
        | Ok Full -> raise Exit (* server already has two players *)
        | Error _ | (exception _) -> (me, state))
      (me, state) (Net.poll conn)
  in
  draw_state ~io:render_io state;
  let status =
    if me = 0 then "Connecting…" else "You are player " ^ string_of_int me
  in
  Text.draw ~io ~size:30 ~color:Color.white ~at:(Point.v 340.0 20.0) status;
  next_frame ~io;
  multiplayer ~io conn ~me state

(* Start menu to pick the game mode. [address] is the editable server address
   (host:port) used for multiplayer; it is threaded through frames so the text
   input keeps its content. *)
let rec menu ~io address =
  Box.fill ~io ~color:Color.black (Window.box ~io);
  Text.draw ~io ~size:60 ~color:Color.white ~at:(Point.v 360.0 200.0) "Volley";
  if Input.is_down ~io (`input_char "f") then
    Window.set_fullscreen ~io (not (Window.get_fullscreen ~io));
  if Input.is_pressed ~io `escape then raise Exit;
  let choice = ref None in
  let address = ref address in
  let _ =
    Ui.window
      ~size:(fun s -> Vec.(2. * s))
      ~io ~at:(Point.v 360.0 400.0)
      (fun [%ui] ->
        if Ui.button [%ui] "Singleplayer" then choice := Some `Single;
        Ui.label [%ui] "Server address:";
        address := Ui.text_input [%ui] !address;
        if Ui.button [%ui] "Multiplayer" then choice := Some `Multi)
  in
  match !choice with
  | Some `Single -> `Single
  | Some `Multi -> `Multi !address
  | None ->
      next_frame ~io;
      menu ~io !address

let main ~io =
  Window.set_size ~io (Size.v 1010. 1020.);
  match menu ~io default_server_address with
  | `Single -> singleplayer ~io initial_state
  | `Multi address ->
      let conn = Net.connect ("ws://" ^ address) in
      multiplayer ~io conn ~me:0 initial_state

let () = Gamelle.run_no_loop main
