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

(* --- Client-side prediction with server reconciliation ---

   The server is authoritative, but waiting a full round trip to see your own
   paddle move feels laggy. So we predict our paddle locally: take the latest
   authoritative paddle from the server and replay every input we have sent that
   the server has not acknowledged yet, applying the same per-frame update the
   server does. Each input carries a [seq]; the server echoes the last [seq] it
   applied, so we know exactly which inputs to replay. The ball and the opponent
   are not predicted — they come straight from the server. *)

(* One frame of our paddle's simulation, matching what the server does for it in
   [step]: move from the input, then keep it inside the world (walls + our half
   divider). The ball/opponent are left out — the paddle is ~1000x heavier than
   the ball, so ignoring that contact is a good approximation. *)
let predict_step ~block paddle input =
  let dt = 1.0 /. 60.0 in
  let gravity = Vec.v 0.0 (1500.0 *. dt) in
  let paddle = update_player ~dt ~gravity ~input ~player:paddle in
  let shape =
    let open Physics.CollisionOp in
    let+ shape = obj paddle.shape and+ _world = obj_list world in
    let+ shape = obj shape and+ _ = obj block in
    shape
  in
  { paddle with shape }

(* Multiplayer client. [server_frame] is the latest server frame seen (tags our
   outgoing inputs and drives the server's lag compensation), [seq] our input
   counter, [pending] the inputs we have sent but the server has not acked yet
   (oldest first), replayed on top of the authoritative state for prediction. *)
let rec multiplayer ~io conn ~me ~server_frame ~seq ~pending state =
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
  let seq = seq + 1 in
  Net.send conn
    (Yojson.Safe.to_string
       (to_server_to_yojson { seq; for_frame = server_frame; input }));
  let pending = pending @ [ (seq, input) ] in
  let me, server_frame, state, ack =
    List.fold_left
      (fun (me, sf, st, ack) msg ->
        match to_client_of_yojson (Yojson.Safe.from_string msg) with
        | Ok (Welcome n) -> (n, sf, st, ack)
        | Ok (State s) -> (me, s.frame, s.state, Some s.ack)
        | Ok Full -> raise Exit (* server already has two players *)
        | Error _ | (exception _) -> (me, sf, st, ack))
      (me, server_frame, state, None)
      (Net.poll conn)
  in
  (* Drop inputs the server has confirmed; keep the rest to replay. Cap the
     backlog so a dead connection can't make us replay an ever-growing list. *)
  let pending =
    match ack with
    | None -> pending
    | Some (a1, a2) ->
        let my_ack = if me = 1 then a1 else a2 in
        List.filter (fun (s, _) -> s > my_ack) pending
  in
  let pending =
    let extra = List.length pending - 120 in
    if extra > 0 then List.filteri (fun i _ -> i >= extra) pending else pending
  in
  (* Predict our paddle: authoritative state + replay of unacked inputs. *)
  let render_state =
    let predict who paddle =
      let block = if me = 1 then block_player1 else block_player2 in
      let predicted =
        List.fold_left
          (fun p (_, inp) -> predict_step ~block p inp)
          paddle pending
      in
      who predicted
    in
    match me with
    | 1 -> predict (fun p -> { state with player1 = p }) state.player1
    | 2 -> predict (fun p -> { state with player2 = p }) state.player2
    | _ -> state
  in
  draw_state ~io:render_io render_state;
  let status =
    if me = 0 then "Connecting…" else "You are player " ^ string_of_int me
  in
  Text.draw ~io ~size:30 ~color:Color.white ~at:(Point.v 340.0 20.0) status;
  next_frame ~io;
  multiplayer ~io conn ~me ~server_frame ~seq ~pending state

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
      multiplayer ~io conn ~me:0 ~server_frame:0 ~seq:0 ~pending:[]
        initial_state

let () = Gamelle.run_no_loop main
