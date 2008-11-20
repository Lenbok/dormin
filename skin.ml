type skin = (float * float * float * int) array
type skel = (int * float array) array
type anim = Qtr.t array
type vertices = float array
type normals = float array
type coords = float array

module SW = struct
external init : bool -> (vertices * normals * coords * skin * string) -> unit
  = "ml_skin_init"

external draw_begin : unit -> unit = "ml_skin_draw_begin"
external draw_end : unit -> unit = "ml_skin_draw_end"

external set_skel : skel -> unit = "ml_skin_set_skel"
external set_anim : anim -> unit = "ml_skin_set_anim"
external anim : unit -> unit = "ml_skin_anim"
end

module HW = struct
external init : bool -> (vertices * normals * coords * skin * string) -> unit
  = "ml_skin_init_vp"

external draw_begin : unit -> unit = "ml_skin_draw_begin_vp"
external draw_end : unit -> unit = "ml_skin_draw_end_vp"

external set_skel : skel -> unit = "ml_skin_set_skel_vp"
external set_anim : anim -> unit = "ml_skin_set_anim_vp"
external anim : unit -> unit = "ml_skin_anim_vp"
external set_text : string -> unit = "ml_skin_set_text_vp"
end

let sw = object (self)
  method init = SW.init
  method draw_begin = SW.draw_begin
  method draw_end = SW.draw_end
  method set_skel = SW.set_skel
  method set_anim = SW.set_anim
  method anim = SW.anim
  method set_text = ()
end;;

let hw path = object (self)
  method init = HW.init
  method draw_begin = HW.draw_begin
  method draw_end = HW.draw_end
  method set_skel = HW.set_skel
  method set_anim = HW.set_anim
  method anim = HW.anim
  method set_text =
    let ic = open_in path in
    let b = Buffer.create 100 in
    begin try
        while true do
          Buffer.add_string b (input_line ic);
          Buffer.add_char b '\n';
        done
      with End_of_file -> close_in ic
      | exn -> close_in ic; raise exn
    end;
    HW.set_text (Buffer.contents b)
end;;

let skin = ref sw;;

let set path =
  if String.length path = 0
  then skin := sw
  else (
    if Glut.extensionSupported "GL_ARB_vertex_program"
    then
      skin := hw path
    else (
      Format.eprintf "GL_ARB_vertex_program not supported falling back to sw@.";
      skin := sw
    )
  )
;;

let init a b = !skin#init a b; !skin#set_text;;
let draw_begin a = !skin#draw_begin a;;
let draw_end a = !skin#draw_end a;;
let set_skel a = !skin#set_skel a;;
let set_anim a = !skin#set_anim a;;
let anim a = !skin#anim a;;
let set_text () = !skin#set_text;;
