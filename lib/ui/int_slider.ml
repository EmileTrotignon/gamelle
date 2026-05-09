module Ui = Ui_backend

let v [%ui] ~min ~max value =
  int_of_float
  @@ Slider.v [%ui] ~min:(float_of_int min) ~max:(float_of_int max)
       (float_of_int value)
