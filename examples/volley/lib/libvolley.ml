open Gamelle

type player = { shape : Physics.t; jumps : int; grounded : bool }

type state = {
  player1 : player;
  player2 : player;
  ball : Physics.t;
  points1 : int;
  points2 : int;
}

(* A player's interaction with the game, expressed in terms of what the player
   wants to do ("move left", "jump") rather than which keys are held. This is
   the unit of input that is shared between the singleplayer and (future)
   multiplayer modes: a client turns raw key events into a [player_input], and
   the simulation only ever consumes [player_input]s. Being declarative and
   tiny, it serializes cleanly for sending over the network. *)
type player_input = { left : bool; right : bool; down : bool; jump : bool }
[@@deriving yojson]

let no_input = { left = false; right = false; down = false; jump = false }
let restitution = 1.0
let player_radius = 80.0
let bottom = 500.0
let horz_speed = 2000.0

let init_ball () =
  Physics.add_velocity (Vec.v (Random.float 30.0 -. 15.0) (-1_000.0))
  @@ Physics.v ~mass:1.0 ~restitution:1.0 ~kind:Movable
       (Shape.circle (Circle.v (Point.v 500.0 0.0) 50.0))

let init_player pos =
  {
    shape =
      Physics.v ~restitution:0.8 ~kind:Movable ~mass:1000.0
        (Shape.circle (Circle.v pos player_radius));
    jumps = 0;
    grounded = false;
  }

let initial_state =
  {
    player1 = init_player (Point.v 200.0 200.0);
    player2 = init_player (Point.v 800.0 200.0);
    ball = init_ball ();
    points1 = 0;
    points2 = 0;
  }

let world =
  [
    Physics.v ~restitution ~kind:Immovable
      (Shape.circle (Circle.v (Point.v 500.0 200.0) 10.0));
    Physics.v ~restitution:0.5 ~kind:Immovable
      (Shape.rect @@ Box.v (Point.v 0.0 (-1500.0)) (Vec.v 1000.0 1010.0));
    Physics.v ~restitution:0.5 ~kind:Immovable
      (Shape.rect @@ Box.v (Point.v 490.0 200.0) (Vec.v 20.0 1000.0));
    Physics.v ~restitution:0.5 ~kind:Immovable
      (Shape.rect @@ Box.v (Point.v (-1000.0) (-1000.0)) (Vec.v 1010.0 1500.0));
    Physics.v ~restitution:0.5 ~kind:Immovable
      (Shape.rect @@ Box.v (Point.v 1000.0 (-1000.0)) (Vec.v 1010.0 1500.0));
    Physics.v ~restitution:0.0 ~kind:Immovable
      (Shape.rect @@ Box.v (Point.v (-1000.0) bottom) (Vec.v 3500.0 1000.0));
  ]

let block_player1 =
  Physics.v ~restitution ~kind:Immovable
  @@ Shape.rect
  @@ Box.v (Point.v 500.0 (-1000.0)) (Vec.v 1000.0 2000.0)

let block_player2 =
  Physics.v ~restitution ~kind:Immovable
  @@ Shape.rect
  @@ Box.v (Point.v (-500.0) (-1000.0)) (Vec.v 1000.0 2000.0)

(* Map raw key events to a declarative [player_input] given a key binding. This
   is the client-side translation step: held keys are continuous actions, while
   [up] triggers a jump only on the frame it is pressed. *)
let read_player_input event ~left ~right ~up ~down =
  {
    left = Input_event.is_pressed event left;
    right = Input_event.is_pressed event right;
    down = Input_event.is_pressed event down;
    jump = Input_event.is_down event up;
  }

let update_player ~dt ~gravity ~input:{ left; right; down; jump }
    ~player:{ shape = player; jumps; grounded } =
  let grounded, player =
    let touching_ground =
      Vec.y (Physics.center player) >= bottom -. player_radius -. 10.0
    in
    if touching_ground && not grounded then
      (true, Physics.(set_rot_velocity 0.0 @@ set_velocity Vec.zero player))
    else (touching_ground, player)
  in
  let player =
    if left then Physics.add_velocity (Vec.v (-.horz_speed *. dt) 0.0) player
    else player
  in
  let player =
    if right then Physics.add_velocity (Vec.v (horz_speed *. dt) 0.0) player
    else player
  in
  let player =
    if down then Physics.add_velocity (Vec.v 0.0 (10_000.0 *. dt)) player
    else player
  in
  let jumps = if grounded then 0 else jumps in
  let player, jumps =
    if jump && jumps < 2 then
      (Physics.add_velocity (Vec.v 0.0 (-60000.0 *. dt)) player, jumps + 1)
    else (player, jumps)
  in
  let player = Physics.add_velocity gravity player in
  let player = Physics.update ~dt player in
  { shape = player; jumps; grounded }

(* Advance both players in a single step from each one's input. The simulation
   side (singleplayer loop or multiplayer server) feeds the two [player_input]s
   here; collision resolution against the world is handled by the caller. *)
let update_players ~dt ~gravity ~input1 ~input2
    ({ player1; player2; _ } as state) =
  {
    state with
    player1 = update_player ~dt ~gravity ~input:input1 ~player:player1;
    player2 = update_player ~dt ~gravity ~input:input2 ~player:player2;
  }
