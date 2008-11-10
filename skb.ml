open Format;;

let qof f = Qtr.make f.(5) f.(6) f.(7) f.(8);;
let vof f = Vec.make f.(1) f.(2) f.(3);;

type t = int * (string * int32 array * float array * int array * float);;

let rt sbuf strtab =
  let d = Array.init 3 (fun n -> Xff.r32 sbuf (n*4)) in
  let f9 = Array.init 9 (fun n -> Xff.rfloat sbuf (12 +n*4)) in
  let h = Array.init 3 (fun n -> Xff.rint sbuf (48 + n*4)) in
  let f = Xff.rfloat sbuf 60 in
  (Xff.rcstrtabent strtab 0 (Int32.to_int d.(0)), d, f9, h, f)
;;

let r1 xff sbufxff =
  if Array.length xff.Xff.sections != 2
  then
    Xff.sbuferr sbufxff 0 "number of xff sections is not 2"
  ;
  let sectpos = xff.Xff.sections.(1).Xff.off in
  let sectbuf = Xff.sbufplus sbufxff sectpos in
  let skbbuf = Xff.sbufplus sectbuf xff.Xff.entry in

  let count1 = Xff.rint skbbuf 4 in
  let _pos1 = Xff.rint skbbuf 8 in
  let tabpos1 = Xff.rint skbbuf 12 in
  let strtab1 = Xff.sbufplus sectbuf tabpos1 in

  let bones = Array.init count1 (fun n ->
    let pos = Xff.rint skbbuf (44 + n*8) in
    let sbuf = Xff.sbufplus sectbuf pos in
    pos + sectpos, rt sbuf strtab1)
  in
  bones
;;

let vertices bones quats =
  let parentinfo = Array.make (Array.length bones + 1) (Qtr.id, Vec.origin) in
  let mapf i (pos, (name, offsets, floats, neighbors, float)) =
    let curq = quats.(i) in
    let curv = vof floats in

    let parent = neighbors.(2) in
    let parentq, parentv = parentinfo.(parent + 1) in

    let v = Qtr.apply parentq curv in
    let v = Vec.add v parentv in

    let q = Qtr.compose curq parentq in
    parentinfo.(i + 1) <- (q, v);
    Vec.elts parentv, Vec.elts v;
  in
  Array.mapi mapf bones;
;;

let sphere =
  let quad = lazy (GluQuadric.create ()) in
  fun origin ->
    GlMat.mode `modelview;
    GlMat.push ();
    GlMat.translate3 (origin);
    let radius = Rend.view.Rend.radial_scale*.0.009 in
    let quad = Lazy.force quad in
    GluQuadric.sphere ~radius ~stacks:5 ~slices:5 ~quad ();
    GlMat.pop ();
;;

let draw bones =
  fun ?(dsphere=false) quats ->
    let rta = vertices bones quats in
    fun () ->
      GlDraw.polygon_mode `both `line;
      GlDraw.color (0., 0., 1.);
      Gl.disable  `depth_test;
      for i = 0 to Array.length bones - 1 do
        let v0, v1 = rta.(i) in
        if dsphere then (
          GlDraw.line_width 0.1;
          sphere v1;
        );
        GlDraw.line_width 2.0;
        GlDraw.begins `lines;
        GlDraw.vertex3 v0;
        GlDraw.vertex3 v1;
        GlDraw.ends ();
      done;
      Gl.enable `depth_test;
;;

let skbquats bones =
  Array.map (fun (_, (_, _, f, _, _)) -> qof f) bones
;;

let animate quats =
  Skin.set_anim quats;
  Skin.anim ();
;;

let func bones anim =
  let posecount, rotations = anim in
  let clip poseno =
    let poseno' = poseno mod posecount in
    if poseno' < 0 then posecount + poseno' else poseno'
  in
  let skeldraw n quats =
    match n mod 3 with
    | 0 -> ()
    | 1 -> draw bones quats ()
    | _ -> draw ~dsphere:true bones quats ()
  in
  let skbquats = skbquats bones in
  let rec subfunc drawindex quats sposeno dposeno t dir =
    let subf
        ?(drawindex=drawindex)
        ?(sposeno=sposeno)
        ?(dposeno=dposeno)
        ?(t=t)
        ?(quats=quats)
        ?(dir=dir) () =
      let drawindex = drawindex mod 3 in
      let hf () =
        [("s", "toggle skeleton (S type)",
         if drawindex = 0 then "off" else string_of_int drawindex)
        ;"B", sprintf "toggle animation direction", string_of_int dir
        ;"f", "forward one frame", sprintf "%d, %f" sposeno t
        ;"b", "backward one frame", sprintf "%d, %f" sposeno t
        ;"", "", "total frames " ^ string_of_int posecount
        ;"r", "go to bind pose", ""
        ;"1,2", "go to first/last frame", ""
        ]
      in
      Rend.Func (subfunc drawindex quats sposeno dposeno t dir, hf)
    in
    let advance quats dir =
      let t = t +. !Rend.slerp_step in
      if t >= 1.0
      then
        let sposeno = dposeno
        and dposeno = clip (dposeno + dir) in
        subf ~quats ~sposeno ~dposeno ~t:0.0 ()
      else
        subf ~quats ~sposeno ~dposeno ~t ()
    in
    function
      | Rend.Char ('n' | 'f' | 'b' as c) ->
          let quats = Anb.interpolated rotations sposeno dposeno t in
          animate quats;
          advance quats
            (match c with
            | 'n' -> dir
            | 'f' -> 1
            | _ -> -1)

      | Rend.Char 'B' ->
          subf ~dir:~-dir ()

      | Rend.Char 'r' ->
          Skin.set_anim skbquats;
          Skin.anim ();
          subf ~drawindex:1 ~quats:skbquats ()

      | Rend.Char ('1' | '2' as c) ->
          let sposeno, dposeno =
            if c = '1'
            then 0, clip 1
            else clip (posecount - 1), 0
          in
          let quats = Anb.exact rotations sposeno in
          animate quats;
          let t = if dir > 0 then 1.0 else 0.0 in
          subf ~quats ~sposeno ~dposeno ~t ~dir ()

      | Rend.Char 's' ->
          subf ~drawindex:(drawindex lxor 1) ()

      | Rend.Char 'S' ->
          subf ~drawindex:(drawindex + 1) ()

      | Rend.Draw ->
          skeldraw drawindex quats;
          subf ()

      | _ ->
          subf ()
  in
  subfunc 0 skbquats 0 (clip 1) 0.0 1
;;

let dummy draw =
  let rec subfunc dodraw =
    let hf () =
      ["s", "toggle skeleton", if dodraw then "on" else "off"]
    in
    function
      | Rend.Draw -> if dodraw then draw (); Rend.Func (subfunc dodraw, hf)
      | Rend.Char 's' -> Rend.Func (subfunc (not dodraw), hf)
      | _ -> Rend.Func (subfunc dodraw, hf)
  in
  subfunc false
;;

let skin bones =
  let skel = Array.map
    (fun (_, (_, _, floats, neighbors, _)) -> (neighbors.(2), floats))
    bones
  in
  Skin.set_skel skel;
;;

let main name =
  let func =
    try
      let name =
        match !Rend.skb_name with
        | Some s -> s
        | None ->
            Filename.chop_extension name ^ ".skb"
      in
      let xff, sbuf = Xff.test2 name in
      let bones = r1 xff sbuf in
      begin match !Rend.anb_names with
      | [] ->
          let quats = skbquats bones in
          dummy (draw bones quats)
      | hd :: tl ->
          let ranb name =
            let xff, sbuf = Xff.test2 (Filename.basename name) in
            let anim = Anb.r xff sbuf in
            anim
          in
          let anim =
            List.fold_left
              (fun accu name -> Anb.append accu (ranb name)) (ranb hd) tl
          in
          skin bones;
          func bones anim;
      end;
    with exn ->
      prerr_endline (Printexc.to_string exn);
      let rec f _ = Rend.Func (f, (fun () -> [])) in f
  in
  Rend.add_func func
;;
