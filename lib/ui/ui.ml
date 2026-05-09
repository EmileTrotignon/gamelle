open Draw_geometry
open Ui_backend
include Widgets

let text_input = Text_input.v
let vscroll = Vscroll.v
let slider = Slider.v
let int_slider = Int_slider.v

let window ~io ?width ?height fn =
  let t = { io = ref io; renderers = []; loc_stack = [] } in
  let ui = (t, "root") in
  let result =
    over ui @@ fun () ->
    draw ui (Box.fill ~color:Gruvbox.Light.bg_s);
    padding ui 10.0 @@ fun () -> vertical ui (fun () -> fn ui)
  in
  match t.renderers with
  | [ single ] ->
      Layout.solve ?width ?height single;
      Ui_backend.clean_old_states ();
      result
  | _ -> assert false

type constrain = Layout.constrain = { min : float; flex : float; max : float }

let update_loc = Ui_backend.update_loc
let nest_loc = Ui_backend.nest_loc

module Custom = struct
  type 'a state = 'a Ui_backend.state
  let value = Ui_backend.state_value
  let update = Ui_backend.state_update
  let get_io = Ui_backend.get_io
  let draw = Ui_backend.draw
  let parent = Ui_backend.parent
  let parent1 = Ui_backend.parent1
  let vclip = Ui_backend.vclip
  let hclip = Ui_backend.hclip
  let horizontal = Widgets.horizontal
  let vertical = Widgets.vertical
  let over = Widgets.over
  module Layout = Layout
  let on_click = Widgets.on_click
  let with_box = Widgets.with_box
  let boxed = Widgets.boxed
  let padding = Widgets.padding
  let center = Widgets.center
  module type Store = Ui_backend.Store
  module State = Ui_backend.State
  let with_state = Ui_backend.with_state
end
