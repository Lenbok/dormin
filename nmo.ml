open Format;;

type subhdr =
    { off : int
    ; count : int
    }

type surf =
    { tricount : int
    ; strcount : int
    ; name : string
    ; hdr1 : (int array * float * int32 * int32) array
    ; hdr2 : (int32 * float * float * int32 array)
    ; hdr3 : int32 array
    }

type surf1 =
    { size : int
    ; surf : int
    ; offs : int
    ; tri_count : int
    ; strip_count : int
    }

type tex =
    { texname : string
    ; nto : (int * int * string)
    ; int5 : int32 array
    ; half1 : int array
    ; w : int
    ; h : int
    }

type geom =
    { vertexa : float array
    ; skin : Skin.skin
    ; normala : float array
    ; uva : float array
    ; colora : string
    ; surfaces : (int list * surf * GlTex.texture_id lazy_t) list
    }

let rsurf1 sbuf =
  { size = Xff.rint sbuf 0
  ; surf = Xff.rint sbuf 8
  ; offs = Xff.rint sbuf 16
  ; tri_count = Xff.rint sbuf 20
  ; strip_count = Xff.rint sbuf 24
  }
;;

let mag i j k s = sqrt (i*.i +. j*.j +. k*.k +. s*.s);;

let verts_in_surf1 surf1 sbuf =
  let sbuf = Xff.sbufplus sbuf surf1.offs in
  let r8 = Xff.r8 sbuf in
  let rec r1 verts pos =
    let a, b, c, d = r8 pos, r8 (pos+1), r8 (pos+2), r8 (pos+3) in
    let pos = pos + 4 in
    let skip pos = r verts pos in
    match d with
    | 0x05 when c = 0 -> skip pos
    | 0x17 when c = 0 -> skip pos
    | 0x65 -> r verts (pos + 4*c)
    | 0x68 when a = 1 -> r (verts + c) (pos + 12*c)
    | 0x68 -> skip (pos + 12*c)
    | 0x6c -> skip (pos + 16*c)
    | 0x6d -> skip (pos + 8*c)
    | 0x6e -> skip (pos + 4*c)
    | 0x00 when a = 0 && b = 0 && c = 0 -> verts
    | _ ->
        let msg = sprintf "geom (a=%x b=%x c=%x d=%x)" a b c d in
        Xff.sbuferr sbuf pos msg
  and r verts pos =
    if pos = surf1.size then verts
    else r1 verts pos
  in
  r 0 12
;;

let app count pos index pos_incr index_incr f =
  let rec g p i count = if count = 0 then () else (
    f p i;
    g (p + pos_incr) (i + index_incr) (count -1)
  )
  in g pos (index*index_incr) count
;;

let rgeom1 start_index surf1 geom sbuf =
  let sbuf = Xff.sbufplus sbuf surf1.offs in
  let r8, r16s, r16, rfloat =
    Xff.r8 sbuf, Xff.r16s sbuf, Xff.r16 sbuf, Xff.rfloat sbuf
  in
  let rec r1 counts index prev_count pos =
    let a, b, c, d = r8 pos, r8 (pos+1), r8 (pos+2), r8 (pos+3) in
    let pos = pos + 4 in
    let skip pos = r counts index prev_count pos in
    let skip2 n = skip (pos+c*n) in
    match d with
    | 0x05 when c = 0 -> skip pos
    | 0x17 when c = 0 -> skip pos

    | 0x65 ->
        app c pos index 4 2 (fun pos index ->
          let u = r16s (pos + 0)
          and v = r16s (pos + 2) in
          let u = float u /. 4096.0
          and v = float v /. 4096.0 in
          geom.uva.(index + 0) <- u;
          geom.uva.(index + 1) <- v;
        );
        skip2 4

    | 0x68 when a = 1 ->
        let index = index + prev_count in
        for i = 0 to pred c do
          let vi = index*3 + i*3
          and pi = pos + i*12 in
          geom.vertexa.(vi + 0) <- rfloat (pi + 0);
          geom.vertexa.(vi + 1) <- rfloat (pi + 4);
          geom.vertexa.(vi + 2) <- rfloat (pi + 8);
        done;
        r (c :: counts) index c (pos + c*12)

    | 0x68 when a = 2 ->
        for i = 0 to pred c do
          let vi = index*3 + i*3 in
          let x = rfloat (pos + i*12 + 0) in
          let y = rfloat (pos + i*12 + 4) in
          let z = rfloat (pos + i*12 + 8) in
          geom.normala.(vi + 0) <- x;
          geom.normala.(vi + 1) <- y;
          geom.normala.(vi + 2) <- z;
        done;
        skip2 12

    | 0x6c when a = 0 -> skip (pos + 16*c)
    | 0x6c ->
        for i = 0 to pred c do
          let pi = pos + i*16 in
          let a = rfloat (pi + 0) in
          let b = rfloat (pi + 4) in
          let c = rfloat (pi + 8) in
          let d = Xff.rint sbuf (pi + 12) in
          geom.skin.(index + i) <- (a,b,c,d);
        done;
        skip2 16

    | 0x6d when a = 2 ->
        for i = 0 to pred c do
          let vi = index*3 + i*3 in
          let x = r16s (pos + i*8 + 0) in
          let y = r16s (pos + i*8 + 2) in
          let z = r16s (pos + i*8 + 4) in
          geom.normala.(vi + 0) <- float x /. 4096.;
          geom.normala.(vi + 1) <- float y /. 4096.;
          geom.normala.(vi + 2) <- float z /. 4096.;
        done;
        skip2 8

    | 0x6d when a = 3 ->
        for i = 0 to pred c do
          let a = r16s (pos + i*8 + 0) in
          let b = r16s (pos + i*8 + 2) in
          let c = r16s (pos + i*8 + 4) in
          let d = r16s (pos + i*8 + 6) in
          let i = float a /. 4096. in
          let j = float b /. 4096. in
          let k = float c /. 4096. in
          let s = float d /. 4096. in
          let _ = i,j,k,s in ()
        done;
        skip2 8

    | 0x6d ->
        skip2 8

    | 0x6e ->
        for i = 0 to pred c do
          let vi = index*4 + i*4 in
          Xff.sbufblt sbuf
            ~dst:geom.colora
            ~src_pos:(pos + i*4)
            ~dst_pos:vi
            ~len:4
          ;
        done;
        skip2 4

    | 0x00 when a = 0 && b = 0 && c = 0 ->
        index + prev_count, counts

    | _ ->
        let msg = sprintf "geom (a=%x b=%x c=%x d=%x)" a b c d in
        Xff.sbuferr sbuf pos msg

  and r counts index prev_count pos =
    if pos = surf1.size
    then index + prev_count, counts
    else r1 counts index prev_count pos
  in
  r [] start_index 0 12
;;

let rtext n sectbuf sbuf =
  if not (Xff.cmp sbuf (`chars "TEX\000"))
  then
    Xff.sbuferr sbuf 0 "invalid TEX signature"
  ;
  let int5 = Array.init 5 (fun n -> Xff.r32 sbuf (4+n*4)) in
  let half2_1 = Array.init 2 (fun n -> Xff.r16 sbuf (24+n*2)) in
  let w = Xff.r16 sbuf 28
  and h = Xff.r16 sbuf 30 in
  let nameoff = int5.(0) in
  let name = (Xff.rcstrtabent sectbuf (Int32.to_int nameoff) 0) in
  let nto =
    let xff, sbuf =
      if true then Xff.test2 (name ^ ".nto")
      else Xff.test2 ("scee_logo_uk.nto")
    in
    let dim = (w, h) in
    Nto.r xff sbuf ~dim ()
  in
  { texname = name
  ; nto = nto
  ; int5 = int5
  ; half1 = half2_1
  ; w = w
  ; h = h
  }
;;

let rsrf n sectbuf sbuf =
  if not (Xff.cmp sbuf (`chars "SRF\000"))
  then
    Xff.sbuferr sbuf 0 "invalid SRF signature"
  ;
  let tricount = Xff.rint sbuf 4
  and stripcount = Xff.rint sbuf 8
  and nameoff = Xff.rint sbuf 12 in
  let hdr1 =
    Array.init 3
      (fun n ->
        let sbuf = Xff.sbufplus sbuf (16 + n*16) in
        let _0 = Array.init 4 (fun n -> Xff.r8 sbuf n) in
        let _1 = Xff.rfloat sbuf 4 in
        let _2 = Xff.r32 sbuf 8 in
        let _3 = Xff.r32 sbuf 12 in
        (_0, _1, _2, _3)
      )
  in
  let hdr2 =
    let sbuf = Xff.sbufplus sbuf (16 + 3*16) in
    let _0 = Xff.r32 sbuf 0 in
    let _1 = Xff.rfloat sbuf 4 in
    let _2 = Xff.rfloat sbuf 8 in
    let _3 = Array.init 5 (fun n -> Xff.r32 sbuf (12 + n*4)) in
    (_0, _1, _2, _3)
  in
  let hdr3 =
    let sbuf = Xff.sbufplus sbuf (16 + 3*16 + 32) in
    Array.init 48 (fun n -> Xff.r32 sbuf (n*4))
  in
  let name = Xff.rcstrtabent sectbuf nameoff 0 in
  { tricount = tricount
  ; strcount = stripcount
  ; name = name
  ; hdr1 = hdr1
  ; hdr2 = hdr2
  ; hdr3 = hdr3
  }
;;

let r xff sbufxff =
  if Array.length xff.Xff.sections != 2
  then
    Xff.sbuferr sbufxff 0 "number of xff sections is not 2"
  ;
  let sectpos = xff.Xff.sections.(1).Xff.off in
  let sectbuf = Xff.sbufplus sbufxff sectpos in
  let nmobuf = Xff.sbufplus sectbuf xff.Xff.entry in
  if not (Xff.cmp nmobuf (`chars "NMO\000"))
  then
    Xff.sbuferr nmobuf 0 "invalid NMO signature"
  ;
  let hdrs = Array.init 5 (fun n ->
    let pos = 0x30 + n*16 in
    let off = Xff.rint nmobuf pos
    and count = Xff.rint nmobuf (pos+4) in
    { off = off
    ; count = count
    }
  )
  in
  let texts =
    Array.init hdrs.(1).count (fun n ->
      let sbuf = Xff.sbufplus sectbuf (hdrs.(1).off + (n*32)) in
      rtext n sectbuf sbuf
    )
  in

  let surfs =
    Array.init hdrs.(2).count (fun n ->
      let sbuf = Xff.sbufplus sectbuf (hdrs.(2).off + (n*288)) in
      rsrf n sectbuf sbuf
    )
  in

  let num_vertices, num_strips =
    let rec calc num_vertices n =
      if n = hdrs.(3).count then num_vertices
      else
        let sbuf = Xff.sbufplus sectbuf (hdrs.(3).off + (n*32)) in
        let surf1 = rsurf1 sbuf in
        let here_verts = verts_in_surf1 surf1 sectbuf in
        calc (num_vertices + here_verts) (n + 1)
    in
    let num_vertices = calc 0 0 in
    let num_strips =
      Array.fold_left
        (fun num_strips surf -> num_strips + surf.strcount) 0 surfs
    in
    (num_vertices, num_strips)
  in

  let surf1s = Array.init hdrs.(3).count (fun n ->
    let sbuf = Xff.sbufplus sectbuf (hdrs.(3).off + (n*32)) in
    rsurf1 sbuf
  )
  in

  let texts =
    Array.map
      (fun text ->
        lazy
          (
            let nto = text.nto in
            let (_, _, data) = nto in

            let id = GlTex.gen_texture () in
            let raw = Raw.of_string data `ubyte in
            let pix = GlPix.of_raw raw `rgba text.w text.h in
            GlTex.bind_texture `texture_2d id;
            GlTex.parameter `texture_2d (`min_filter `linear);
            GlTex.parameter `texture_2d (`mag_filter `linear);
            GlTex.parameter `texture_2d (`wrap_s `repeat);
            GlTex.parameter `texture_2d (`wrap_t `repeat);
            GlTex.image2d pix;
            id
          )
      ) texts
  in

  let geom =
    { vertexa = Array.make (num_vertices*3) 0.0
    ; normala = Array.create (num_vertices*3) 0.0
    ; colora = String.create (num_vertices*4)
    ; uva = Array.make (num_vertices*2) 0.0
    ; skin = Array.create num_vertices (0., 0., 0., 0)
    ; surfaces = []
    }
  in
  let _, surfaces =
    Array.fold_left
      (fun (last_index, countss) surf1 ->
        let index, counts = rgeom1 last_index surf1 geom sectbuf in
        let surf = surfs.(surf1.surf) in
        let _, _, _, texindex = surf.hdr1.(1) in
        let text = texts.(Int32.to_int texindex) in
        (index, (List.rev counts, surf, text) :: countss)
      ) (0, []) surf1s
  in
  { geom with surfaces = List.rev surfaces }
;;

let draw geom =
  let l = lazy (Skin.init (geom.vertexa, geom.normala, geom.uva, geom.skin)) in
  fun textures lighting solid () ->
    let () = Lazy.force l in
    if true then (
      if textures then (
        Gl.enable `texture_2d;
      )
      else (
        GlDraw.line_width 1.0;
        GlDraw.color (1., 1., 1.);
      );
      if lighting then (
        Gl.enable `lighting;
        Gl.enable `light0;
        Gl.enable `normalize;
        GlTex.env (`mode `modulate);
        GlLight.light_model (`two_side false);
        GlDraw.shade_model `smooth;
      ) else (
        GlTex.env (`mode `replace);
      );
      if solid then (
        GlDraw.polygon_mode `front `fill;
        GlDraw.polygon_mode `back `fill;
      )
      else (
        GlDraw.polygon_mode `front `line;
        GlDraw.polygon_mode `back `line;
      );

      Skin.draw_begin ();

      let rec f last_index surf = function
        | [] -> last_index
        | count :: rest ->
            GlArray.draw_arrays `triangle_strip last_index count;
            f (last_index + count) surf rest
      and g last_index = function
        | [] -> ()
        | (counts, surf, id) :: rest ->
            let texid = Lazy.force id in
            GlTex.bind_texture `texture_2d texid;
            let last_index = f last_index surf counts in
            g last_index rest
      in
      g 0 geom.surfaces;

      Skin.draw_end ();
      Gl.disable `texture_2d;
      Gl.disable `lighting;
      Gl.disable `light0;
      GlDraw.shade_model `flat;
    );
;;

let func geom =
  let draw = draw geom in
  let rec subfunc dodraw textures lighting solid = function
    | Rend.Char 't' ->
        Rend.Func (subfunc dodraw (not textures) lighting solid)
    | Rend.Char 'l' ->
        Rend.Func (subfunc dodraw textures (not lighting) solid)
    | Rend.Char 'w' ->
        Rend.Func (subfunc dodraw textures lighting (not solid))
    | Rend.Char 'm' ->
        Rend.Func (subfunc (not dodraw) textures lighting solid)
    | Rend.Draw ->
        if dodraw then draw textures lighting solid ();
        Rend.Func (subfunc dodraw textures lighting solid)
    | Rend.Char _ ->
        Rend.Func (subfunc dodraw textures lighting solid)
  in
  subfunc true false false false
;;

let _ =
  let name =
    match !Rend.nmo_name with
    | None -> failwith "must supply model name"
    | Some s -> s
  in
  let x, sbuf = Xff.test2 name in
  let geom = r x sbuf in
  let minmax =
    let rec f ((minx, maxx, miny, maxy, minz, maxz) as minmax) i =
      if i >= Array.length geom.vertexa then minmax
      else
        let x = geom.vertexa.(i+0) in
        let y = geom.vertexa.(i+1) in
        let z = geom.vertexa.(i+2) in
        let minmax =
          min minx x, max maxx x,
          min miny y, max maxy y,
          min minz z, max maxz z
        in
        f minmax (i + 3)
    in
    let x = geom.vertexa.(0) in
    let y = geom.vertexa.(1) in
    let z = geom.vertexa.(2) in
    f (x, y, z, x, y, z) 3
  in
  Skb.main name;
  Rend.add_func (func geom);
  Rend.init minmax;
  Rend.main ()
;;
