module type State = sig
  type t
  type input

  val step : dt:float -> inputs:input array -> t -> t
end

module type Input = sig
  type t

  val null : t
  val assume_next : t -> t
end

module type S = sig
  type state
  type input

  type t
  (** netcode data. Contains game {!type-state}s, and {!type-input}s, indexed by
      timestamps.

      Same datastructure can be used on both the client and the server. Assuming
      the server has authority, the client is going to use {!assert_state} and
      the server {!insert_input}.*)

  val init : player_count:int -> timestamp:float -> state -> t

  val insert_input : timestamp:float -> (int * input) option -> t -> t
  (** [insert_input ~timestamp i nc] inserts input [i] into netcode [nc].

      If [i] is [None], then the insertion is treated as a simulated frame where
      all inputs are assument to stay the same (using [Input.assume_next]).

      If [i] is [Some (p, i')] then the inputs are simulated with
      [Input.assume_next] for all players except [p]. *)

  val assert_state : timestamp:float -> state -> t -> t
  (** [assert_state ~timestamp s nc -> t] asserts that the state [s] was the
      correct one at time [timestamp]. All inputs and states dating from before
      the assertions are thrown out, and the simulation is re-run using [s] as
      the initial state.

      Intended for use on parties that don't have authority. *)

  val prune : int -> t -> t
  (** [prune n nc] return [nc] with a maximum of [n] frames.

      Intended for use on the party with authority that never uses
      [assert_state]. *)

  val state : t -> state
end

module Make (Input : Input) (State : State with type input = Input.t) :
  S with type input = Input.t and type state = State.t = struct
  type state = State.t
  type input = Input.t

  module Fmap = Map.Make (Float)

  type frame = {
    state : State.t;
    input : (int * Input.t) option (*todo should be a list*);
    inputs : Input.t array;
  }

  type t = {
    frames : frame Fmap.t;
    first_state : State.t;
    first_inputs : Input.t array;
    first_timestamp : float;
    last_state : State.t;
  }

  let state (m : t) = m.last_state

  let last_timestamp t =
    if Fmap.is_empty t.frames then t.first_timestamp
    else fst (Fmap.max_binding t.frames)

  let last_inputs t =
    if Fmap.is_empty t.frames then t.first_inputs
    else (snd (Fmap.max_binding t.frames)).inputs

  let init ~player_count ~timestamp state =
    let inputs = Array.make player_count Input.null in
    {
      frames = Fmap.empty;
      first_state = state;
      first_inputs = inputs;
      first_timestamp = timestamp;
      last_state = state;
    }

  (* Longest [dt] a single [State.step] may take. The server advances the
   authoritative simulation once per input packet, with [dt] equal to the
   real-time gap between packets (see [insert_input] callers); under network
   jitter that gap can spike to hundreds of milliseconds. A single step that
   large moves a player several world units at once, letting it tunnel clean
   through a thin wall before collision push-out ever sees an overlap. *)
  let max_step = 1. /. 60.

  (* Step [state] forward by [dt] holding [inputs] fixed, but never more than
   [max_step] per collision resolution: a large gap is split into 60Hz
   sub-steps, reproducing what a steady 60fps feed would have simulated
   (the held inputs replayed frame by frame) without tunneling. *)
  let step ~dt ~inputs state =
    let n = max 1 (int_of_float (ceil (dt /. max_step))) in
    let sub = dt /. float_of_int n in
    let rec loop i state =
      if i <= 0 then state else loop (i - 1) (State.step ~dt:sub ~inputs state)
    in
    loop n state

  let tick_input inputs input =
    match input with
    | None -> Array.map Input.assume_next inputs
    | Some (player, input) ->
        Array.mapi
          (fun i elt -> if i = player then input else Input.assume_next elt)
          inputs

  let run_from_start
      { frames; first_state; first_inputs; first_timestamp; last_state = _ } =
    let _last_timestamp, last_state, _last_inputs, frames =
      Fmap.fold
        begin fun
          timestamp
          { state = _; input; inputs = _ }
          ( previous_timestamp,
            previous_state,
            (previous_inputs : Input.t array),
            frames )
        ->
          let dt = timestamp -. previous_timestamp in
          let state = step ~dt ~inputs:previous_inputs previous_state in
          let inputs = tick_input previous_inputs input in
          let frames = Fmap.add timestamp { state; input; inputs } frames in
          (timestamp, state, inputs, frames)
        end
        frames
        (first_timestamp, first_state, first_inputs, frames)
    in
    { frames; first_inputs; first_state; first_timestamp; last_state }

  let insert_input ~timestamp input
      ({ frames; first_inputs; first_state; first_timestamp; last_state } as t)
      =
    let before, at, after = Fmap.split timestamp frames in
    assert (Option.is_none at);
    let after_first_timestamp, after_first_inputs, after_first_state =
      if Fmap.is_empty before then (first_timestamp, first_inputs, first_state)
      else
        let ( before_last_timestamp,
              {
                state = before_last_state;
                input = _;
                inputs = before_last_inputs;
              } ) =
          Fmap.max_binding before
        in
        (before_last_timestamp, before_last_inputs, before_last_state)
    in
    let after =
      Fmap.add timestamp
        { state = last_state; input; inputs = after_first_inputs }
        after
    in
    let after =
      run_from_start
        {
          frames = after;
          first_inputs = after_first_inputs;
          first_state = after_first_state;
          first_timestamp = after_first_timestamp;
          last_state;
        }
    in
    let frames = Fmap.union (fun _ -> assert false) before after.frames in
    { t with frames; last_state = after.last_state }

  let assert_state ~timestamp state t =
    let before, _, frames = Fmap.split timestamp t.frames in
    let first_timestamp = last_timestamp { t with frames = before }
    and first_inputs = last_inputs { t with frames = before } in
    run_from_start
      { t with frames; first_inputs; first_timestamp; first_state = state }

  let prune n m =
    let total = Fmap.cardinal m.frames in
    if total <= n then m
    else
      let bds = Fmap.bindings m.frames in
      let rec loop bds i =
        match bds with
        | [] -> assert false
        | elt :: bds when i = 0 -> (elt, bds)
        | _ :: bds -> loop bds (i - 1)
      in
      let (last_before_timestamp, last_before), after = loop bds (total - n) in
      {
        frames = Fmap.of_list after;
        first_state = last_before.state;
        first_inputs = last_before.inputs;
        first_timestamp = last_before_timestamp;
        last_state = m.last_state;
      }
end
