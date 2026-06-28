open Gamelle
open Libvolley

let rec loop ~io ({ ball; _ } as state) =
  let state =
    let io = View.translate (Vec.v 0.0 500.0) io in
    Box.fill ~io ~color:Color.black (Window.box ~io);
    if Input.is_down ~io (`input_char "f") then
      Window.set_fullscreen ~io (not (Window.get_fullscreen ~io));
    if Input.is_pressed ~io `escape then raise Exit
    else if Input.is_down ~io (`input_char "r") then initial_state
    else if Vec.y (Physics.center ball) > 440.0 then
      if Vec.x (Physics.center ball) < 500.0 then
        { state with ball = init_ball (); points2 = state.points2 + 1 }
      else { state with ball = init_ball (); points1 = state.points1 + 1 }
    else
      let dt = dt ~io in
      let gravity = Vec.v 0.0 (1500.0 *. dt) in
      let ball = Physics.add_velocity gravity ball in
      let ball = Physics.update ~dt ball in
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
      let { player1; player2; _ } =
        update_players ~dt ~gravity ~input1 ~input2 state
      in
      let open Physics.CollisionOp in
      let+ player1_shape = obj player1.shape
      and+ player2_shape = obj player2.shape
      and+ ball = obj ball
      and+ _world = obj_list world in
      let+ player1_shape = obj player1_shape and+ _ = obj block_player1 in
      let+ player2_shape = obj player2_shape and+ _ = obj block_player2 in
      let player1 = { player1 with shape = player1_shape } in
      let player2 = { player2 with shape = player2_shape } in
      List.iter (Physics.fill ~io ~color:Color.white) world;
      Physics.fill ~io ~color:Color.blue player1.shape;
      Physics.fill ~io ~color:Color.blue player2.shape;
      Physics.fill ~io ~color:Color.red ball;
      List.iter (Physics.draw ~io) world;
      Physics.draw ~io player1.shape;
      Physics.draw ~io player2.shape;
      Physics.draw ~io ball;
      Text.draw ~io ~size:40 ~color:Color.white
        (string_of_int state.points1)
        ~at:(Point.v 20.0 10.0);
      Text.draw ~io ~size:40 ~color:Color.white
        (string_of_int state.points2)
        ~at:(Point.v 960.0 10.0);
      { state with player1; player2; ball }
  in
  next_frame ~io;
  loop ~io state

let rec splash_screen ~io frame_number =
  if frame_number / 8 mod 4 <> 0 then
    Text.draw ~io ~size:30 ~at:Vec.zero "press space to play volley";
  if Input.is_down ~io (`input_char "f") then
    Window.set_fullscreen ~io (not (Window.get_fullscreen ~io));
  if Input.is_down ~io `space then ()
  else (
    next_frame ~io;
    splash_screen ~io (frame_number + 1))

let main ~io =
  Window.set_size ~io (Size.v 1010. 1020.);
  splash_screen ~io 0;
  loop ~io initial_state

let () = Gamelle.run_no_loop main
