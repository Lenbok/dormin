type skin = (float * float * float * int) array
type skel = (int * float array) array
type anim = Qtr.t array
type vertices = float array
type normals = float array
type coords = float array

module SW = struct
  type t
  external init
    : string -> bool -> (vertices * normals * coords * skin * string) -> t
    = "ml_skin_init"

  external draw_begin : t -> unit = "ml_skin_draw_begin"
  external draw_end : t -> unit = "ml_skin_draw_end"

  external set_skel : t -> skel -> unit = "ml_skin_set_skel"
  external set_anim : t -> anim -> unit = "ml_skin_set_anim"
  external anim : t -> unit = "ml_skin_anim"
  external set_parent : t -> t -> int -> unit = "ml_skin_set_parent"
  external stl : t -> int -> int -> unit = "ml_skin_stl"
  external stl_begin : t -> unit = "ml_skin_stl_begin"
  external stl_end : t -> unit = "ml_skin_stl_end"
end

let set path = assert false;;

let init = SW.init;;
let draw_begin = SW.draw_begin;;
let draw_end = SW.draw_end;;
let set_skel = SW.set_skel;;
let set_anim = SW.set_anim;;
let anim = SW.anim;;
let set_text () = ();;
let set_parent = SW.set_parent;;
let stl = SW.stl;;
let stl_begin = SW.stl_begin;;
let stl_end = SW.stl_end;;
