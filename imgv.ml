type view =
    { mutable w : int
    ; mutable h : int
    ; mutable tw : float
    ; mutable th : float
    }
;;

let view = { w = -1; h = -1; tw = 0.0; th = 0.0; };;

let setup () =
  GlDraw.viewport 0 0 view.w view.h;
  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.mode `modelview;
  GlMat.load_identity ();
  GluMat.ortho2d ~x:(0.0, view.tw) ~y:(0.0, view.th);
;;

let reshape ~w ~h =
  view.w <- w;
  view.h <- h;
  setup ();
;;

let keyboard ~key ~x ~y =
  begin match Char.chr key with
  | '\027' | 'q' -> exit 0
  | _ -> ()
  end;
  setup ();
  Glut.postRedisplay ();
;;

let draw what func =
  GlDraw.begins what;
  func ();
  GlDraw.ends ();
;;

let display () =
  GlClear.clear [`color];

  draw `quads (fun () ->
    GlTex.coord2 (0.0, 0.0);
    GlDraw.vertex2 (0.0, view.th);

    GlTex.coord2 (1.0, 0.0);
    GlDraw.vertex2 (view.tw, view.th);

    GlTex.coord2 (1.0, 1.0);
    GlDraw.vertex2 (view.tw, 0.0);

    GlTex.coord2 (0.0, 1.0);
    GlDraw.vertex2 (0.0, 0.0);
  );
  Glut.swapBuffers ();
;;

let main =
  let level = ref 0 in
  let nto_name = ref None in
  let setsome r s = r := Some s in
  let spec =
    ["-index", Arg.Set_string Xff.index_path, " <path> to index"
    ;"-base", Arg.String (setsome Xff.base_path), " <directory> base"
    ;"-level", Arg.Set_int level, " <level> mipmap level"
    ]
  in
  Arg.parse (Arg.align spec) (setsome nto_name) "Usage: imgv image.nto";
  let nto_name =
    match !nto_name with
    | None -> failwith "NTO name not specified"
    | Some s -> Filename.basename s
  in
  let xff, sbuf = Xff.test2 nto_name in
  let nto = Nto.r xff sbuf in
  let image2d level (w, h, data) =
    let id = GlTex.gen_texture () in
    Gl.enable `texture_2d;
    GlTex.bind_texture `texture_2d id;
    GlTex.env (`mode `replace);
    GlTex.parameter `texture_2d (`mag_filter `linear);
    GlTex.parameter `texture_2d (`min_filter `linear);
    GlTex.parameter `texture_2d (`wrap_s `repeat);
    GlTex.parameter `texture_2d (`wrap_t `repeat);
    let raw = Raw.of_string data `ubyte in
    let pix = GlPix.of_raw raw `rgba w h in
    GlTex.image2d ~level pix
  in
  let level =
    if !level < 0 || !level >= Array.length nto
    then (
      failwith (
        Printf.sprintf "invalid mipmap level %d, must be in [0..%d]"
          !level
          (pred (Array.length nto))
      );
    )
    else
      !level
  in
  let (w, h, img) as nto = nto.(level) in
  view.w <- w;
  view.h <- h;
  view.tw <- float w;
  view.th <- float h;
  let _ = Glut.init Sys.argv in
  let () = Glut.initDisplayMode ~depth:true ~double_buffer:true () in
  let () = Glut.initWindowSize w h in
  let title = Printf.sprintf "%s (%dx%d)" nto_name w h in
  let _ = Glut.createWindow title in
  let () = Glut.displayFunc display in
  let () = Glut.reshapeFunc reshape in
  let () = Glut.keyboardFunc keyboard in
  image2d 0 nto;
  let () = Glut.mainLoop () in
  ()
;;
