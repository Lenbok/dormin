class type draw = object
  method draw: unit
  method char: char -> draw
  method help: (string * string * string) list
end;;

let try_vbo = ref true
let nmo_name = ref None
let anb_names = ref []
let skb_name = ref None
let mipmaps = ref false
let slerp_step = ref 1.0

type view =
    { mutable w : int
    ; mutable h : int
    ; mutable rotx : float
    ; mutable roty : float
    ; mutable rotz : float
    ; mutable zoom : float
    ; mutable center : (float * float * float)
    ; mutable radial_scale : float
    ; mutable objs : draw list
    ; mutable persp : bool
    ; mutable last_time : float
    ; mutable animated : bool
    ; mutable dumpchan : out_channel Lazy.t
    ; mutable dodump : bool
    ; mutable aincr : float
    ; mutable roteye: bool
    ; mutable sphere : bool
    ; mutable help : bool
    ; mutable x : int
    ; mutable y : int
    ; mutable mtype : [`none|`zoom|`rotate|`move]
    ; mutable transl : (float * float * float)
    ; mutable alpha : float
    ; mutable ambient : float
    ; mutable diffuse : float
    }

let view =
  { w = 0; h = 0
  ; rotx = 0.0; roty = 0.0; rotz = 0.0
  ; center = (0.0, 0.0, 0.0)
  ; radial_scale = 0.0
  ; zoom = 1.0
  ; objs = []
  ; persp = true
  ; last_time = 0.0
  ; animated = false
  ; dumpchan = lazy (open_out_bin "dump.rgb")
  ; dodump = false
  ; aincr = 3.0
  ; roteye = true
  ; sphere = false
  ; help = false
  ; x = 0
  ; y = 0
  ; mtype = `none
  ; transl = (0.0, 0.0, 0.0)
  ; alpha = 0.04
  ; ambient = 1.3
  ; diffuse = 0.5
  }
;;

let mapchar c = view.objs <- List.map (fun draw -> draw#char c) view.objs;;
let appdraw () = List.iter (fun draw -> draw#draw) view.objs;;
let deg2rad deg = deg /. 180.0 *. acos ~-.1.;;

let center_and_radial_scale (minx, maxx, miny, maxy, minz, maxz) =
  let xc = (maxx +. minx) /. 2.0 in
  let yc = (maxy +. miny) /. 2.0 in
  let zc = (maxz +. minz) /. 2.0 in
  let rs =
    let rs = maxx -. minx in
    let rs = max rs (maxy -. miny) in
    let rs = max rs (maxz -. minz) in
    rs
  in
  if false
  then (
    Format.eprintf "x (% f, % f)@." minx maxx;
    Format.eprintf "y (% f, % f)@." miny maxy;
    Format.eprintf "z (% f, % f)@." minz maxz;
    Format.eprintf "c (% f, % f, % f)@." xc yc zc;
    Format.eprintf "rs %f@." rs;
  );
  ((xc, yc, zc), rs)
;;

let help () =
  let font = Glut.BITMAP_HELVETICA_18 in
  let draw_string x y s =
    GlPix.raster_pos ~x ~y ();
    String.iter (fun c -> Glut.bitmapCharacter ~font ~c:(Char.code c)) s
  in
  GlMat.mode `projection;
  GlMat.push ();
  GlMat.load_identity ();
  GlMat.mode `modelview;
  GlMat.push ();
  GlMat.load_identity ();
  GlMat.ortho
    ~x:(0.0, float view.w)
    ~y:(0.0, float view.h)
    ~z:(-1., 1.)
  ;

  Gl.disable `depth_test;
  Gl.disable `alpha_test;

  GlDraw.polygon_mode `both `fill;
  Gl.enable `blend;
  GlFunc.blend_func `src_alpha `one_minus_src_alpha;
  GlDraw.color (0., 0., 0.) ~alpha:0.3;
  GlDraw.rect (0., 0.) (float view.w, float view.h);
  Gl.disable `blend;

  GlDraw.color (1., 1., 1.);
  let rec loop row = function
    | [] -> ()
    | (s, s2, s3) :: rest ->
        let y = view.h - row * 18 - 2 in
        let x = if row = 1 then 0.0 else 5.0 in
        draw_string (x+.5.0) (float y) s;
        draw_string (x+.105.0) (float y) s2;
        draw_string (x+.345.0) (float y) s3;
        loop (row+1) rest
  in
  let help =
    let onoff b = if b then "on" else "off" in
    let angles =
      Printf.sprintf "% f, % f, % f" view.rotx view.roty view.rotz
    in
    [("Keys (h toggles this screen):", "", "")
    ;"e", "toggle eye/model rotation", if view.roteye then "eye" else "model"
    ;"a", "toggle animation", onoff view.animated
    ;"o", "toggle bounding sphere", onoff view.sphere
    ;"d", "dump images to dump.rgb", onoff view.dodump
    ;"q, ESC", "quit", ""
    ;"z,x,arrows", "rotate", angles
    ;"0,9", "zoom", Printf.sprintf "%f" view.zoom
    ;"< , >", "decrease/increase alpha", Printf.sprintf "%1.2f" view.alpha
    ;"[ , ]", "decrease/increase slerp step", Printf.sprintf "%2.1f" !slerp_step
    ;"3,4", "decrease/increase ambient", Printf.sprintf "%2.1f" view.ambient
    ;"5,6", "decrease/increase diffuse", Printf.sprintf "%2.1f" view.diffuse
    ;"","",""
    ]
  in
  let help =
    List.fold_left (fun accu draw -> accu @ draw#help) help view.objs
  in
  loop 1
    (help @
        ["", "", ""
        ;"Move mouse while holding left button pressed to rotate model", "", ""
        ;"Move mouse while holding right button pressed to zoom", "", ""
        ;"Move mouse while holding left button and shift pressed to move model", "", ""
        ;"",
        (let tx, ty, tz = view.transl in
          Printf.sprintf "translation % f, % f, % f" tx ty tz),
        ""
        ])
  ;

  Gl.enable `depth_test;
  Gl.enable `alpha_test;
  GlMat.pop ();
  GlMat.mode `projection;
  GlMat.pop ();
;;

let display () =
  GlClear.color (0.5, 0.5, 0.5) ~alpha:1.0;
  GlClear.clear [`color; `depth];
  GlDraw.color (0.0, 0.0, 0.0);
  GlFunc.alpha_func `greater view.alpha;

  if view.sphere then (
    let cx, cy, cz = view.center in
    let cx = -.cx and cy = -.cy and cz = -.cz in
    GlDraw.line_width 1.0;
    GlMat.mode `modelview;
    GlMat.push ();
    GlMat.translate3 (cx, cy, cz);
    GlDraw.polygon_mode `back `line;
    GlDraw.polygon_mode `front `line;
    Gl.disable `texture_2d;
    GluQuadric.sphere ~radius:(0.7*.view.radial_scale) ~stacks:25 ~slices:25 ();
    GlMat.pop ();
  );

  appdraw ();
  if view.help then help ();
  Glut.swapBuffers ();

  if view.dodump then (
    let pix = GlPix.read 0 0 view.w view.h `rgb `ubyte in
    let raw = GlPix.to_raw pix in
    let pitch = view.w * 3 in
    let size = view.h * pitch in
    let s = Raw.gets_string raw 0 size in
    let dc = Lazy.force view.dumpchan in
    let rec loop pos =
      let pos = pos - pitch in
      if pos < 0 then ()
      else (
        output dc s pos pitch;
        loop pos
      )
    in
    loop size;
  );
;;

let get_eye_and_up () =
  if not view.roteye
  then
    (0.0, 0.0, 2.0), (0.0, 1.0, 0.0)
  else
    let q =
      let rx = deg2rad view.rotx
      and ry = deg2rad view.roty
      and rz = deg2rad view.rotz in
      Qtr.from_euler rz ~-.ry rx
    in
    let v = Qtr.apply q (Vec.make 0.0 0.0 2.0) in
    let u = Qtr.apply q (Vec.make 0.0 1.0 0.0) in
    Vec.elts v, Vec.elts u
;;

let setup w h =
  view.w <- w;
  view.h <- h;
  GlDraw.viewport 0 0 w h;

  let rs = view.zoom /. view.radial_scale in

  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.translate3 view.transl;
  GluMat.perspective
    ~fovy:45.0
    ~aspect:(float w /. float h)
    ~z:(0.1, 10.)
  ;

  GlMat.mode `modelview;
  GlMat.load_identity ();

  let eye, up = get_eye_and_up () in
  GluMat.look_at
    ~eye
    ~center:(0.0, 0.0, 0.0)
    ~up
  ;

  if not view.roteye then (
    GlMat.rotate ~angle:view.rotx ~x:1.0 ();
    GlMat.rotate ~angle:view.roty ~y:~-.1.0 ();
    GlMat.rotate ~angle:view.rotz ~z:1.0 ();
  );

  GlMat.scale3 (-.rs, rs, rs);
  GlMat.translate3 view.center;
;;

let reshape ~w ~h =
  setup w h;
;;

let idle () =
  let deadline = view.last_time +. 0.04 in
  let currtime = Unix.gettimeofday () in
  if deadline > currtime
  then
    let _ = Unix.select [] [] [] (deadline -. currtime) in
    view.last_time <- Unix.gettimeofday ()
  else
    view.last_time <- view.last_time +. 0.04
  ;
  mapchar 'n';
  Glut.postRedisplay ();
;;

let keyboard ~key ~x ~y =
  begin match Char.chr key with
  | '\027' | 'q' -> exit 0
  | '9' -> view.zoom <- view.zoom +. 0.05
  | '0' -> view.zoom <- view.zoom -. 0.05
  | 'z' -> view.roty <- view.roty +. view.aincr
  | 'x' -> view.roty <- view.roty -. view.aincr
  | 'd' -> view.dodump <- not view.dodump
  | 'e' -> view.roteye <- not view.roteye
  | 'o' -> view.sphere <- not view.sphere;
  | 'h' -> view.help <- not view.help
  | 'a' ->
      if view.animated
      then (
        view.animated <- false;
        Glut.idleFunc None
      )
      else (
        view.animated <- true; view.
        last_time <- Unix.gettimeofday ();
        Glut.idleFunc (Some idle)
      )
  | ('f' | 'b') as c when not view.animated -> mapchar c;
  | '<' -> view.alpha <- max (view.alpha -. 0.01) 0.0;
  | '>' -> view.alpha <- min (view.alpha +. 0.01) 1.0;
  | '[' -> slerp_step := max (!slerp_step -. 0.1) 0.0;
  | ']' -> slerp_step := min (!slerp_step +. 0.1) 1.0;
  | '3' -> view.ambient <- view.ambient -. 0.1;
  | '4' -> view.ambient <- view.ambient +. 0.1;
  | '5' -> view.diffuse <- view.diffuse -. 0.1;
  | '6' -> view.diffuse <- view.diffuse +. 0.1;
  | c -> mapchar c;
  end;
  setup view.w view.h;
  Glut.postRedisplay ();
;;

let special ~key ~x ~y =
  begin match key with
  | Glut.KEY_LEFT  -> view.rotz <- view.rotz +. view.aincr
  | Glut.KEY_RIGHT -> view.rotz <- view.rotz -. view.aincr
  | Glut.KEY_UP    -> view.rotx <- view.rotx -. view.aincr
  | Glut.KEY_DOWN  -> view.rotx <- view.rotx +. view.aincr
  | _ -> ()
  end;
  setup view.w view.h;
  Glut.postRedisplay ();
;;

let motion ~x ~y =
  let dx = (x - view.x) in
  let dy = (y - view.y) in
  view.x <- x;
  view.y <- y;
  match view.mtype with
  | `move ->
      let x, y, z = view.transl in
      let dx = float dx /. 100.0
      and dy = float dy /. 100.0 in
      view.transl <- (x +. dx, y -. dy, z);
      setup view.w view.h;
      Glut.postRedisplay ();
  | `rotate ->
      view.rotx <- view.rotx +. float dy;
      view.roty <- view.roty -. float dx;
      setup view.w view.h;
      Glut.postRedisplay ();
  | `zoom ->
      view.zoom <- view.zoom +. (float dy /. 50.);
      setup view.w view.h;
      Glut.postRedisplay ();
  | `none ->
      ()
;;

let mouse ~button ~state ~x ~y =
  if button = Glut.LEFT_BUTTON
  then (
    if state = Glut.DOWN
    then (
      view.x <- x;
      view.y <- y;
      view.mtype <-
        if Glut.getModifiers () = Glut.active_shift
        then `move else `rotate;
    )
    else view.mtype <- `none;
  )
  else if button = Glut.RIGHT_BUTTON
  then (
    if state = Glut.DOWN
    then (
      view.x <- x;
      view.y <- y;
      view.mtype <- `zoom;
    )
    else view.mtype <- `none;
  );
;;

let main () =
  let w = 704
  and h = 576 in
  let _ = Glut.init Sys.argv in
  let () = Glut.initDisplayMode ~depth:true ~double_buffer:true () in
  let () = Glut.initWindowSize w h in
  let _ = Glut.createWindow "rend (press 'h' to get help)" in
  Gl.enable `depth_test;
  Gl.enable `alpha_test;
  let () = Glut.displayFunc display in
  let () = Glut.reshapeFunc reshape in
  let () = Glut.keyboardFunc keyboard in
  let () = Glut.specialFunc special in
  let () = Glut.mouseFunc mouse in
  let () = Glut.motionFunc motion in
  mapchar '\000';                       (* bootstrap *)
  let () = Glut.mainLoop () in
  ()
;;

let add_obj draw =
  view.objs <- draw :: view.objs;
;;

let init minmax =
  let (cx, cy, cz), rs = center_and_radial_scale minmax in
  view.center <- (-.cx, -.cy, -.cz);
  view.radial_scale <- rs;
;;

let _ =
  let setsome r s = r := Some s in
  let spec =
    ["-slice", Arg.String Slice.openslice, "<path> of file to slice data to"
    ;"-index", Arg.Set_string Xff.index_path, "<path> of index"
    ;"-base", Arg.String (setsome Xff.base_path), "<directory> base"
    ;"-sstep", Arg.Set_float slerp_step, "<float> slerp step"
    ;"-novbo", Arg.Clear try_vbo, " do not use vertex buffer objects"
    ;("-skb", Arg.String (setsome skb_name),
     "<name> use specified skb instead of guessing")
    ;"-mipmaps", Arg.Set mipmaps, " use mipmaps"
    ]
  in
  Arg.parse (Arg.align spec)
    (fun s ->
      if !nmo_name != None
      then anb_names := s :: !anb_names
      else nmo_name := Some s;
    )
    "Usage: dormin [options] model.nmo [animation.anb ...]"
;;
