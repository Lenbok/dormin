type skin = (float * float * float * int) array
type skel = (int * float array) array
type anim = Qtr.t array

external init : (float array * float array * float array * skin) -> unit
  = "ml_skin_init"

external draw_begin : unit -> unit = "ml_skin_draw_begin"
external draw_end : unit -> unit = "ml_skin_draw_end"

external set_skel : skel -> unit = "ml_skin_set_skel"
external set_anim : anim -> unit = "ml_skin_set_anim"
external anim : unit -> unit = "ml_skin_anim"
