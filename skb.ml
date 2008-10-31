open Format;;

let xyzofvec v = (v.Vec.x, v.Vec.y, v.Vec.z)

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

let vertices bones =
  let parentinfo = Array.make (Array.length bones + 1) (Qtr.id, Vec.origin) in
  let mapf i (_pos, (_name, _offsets, floats, neighbors, _float)) =
    let curq = qof floats in
    let curv = vof floats in

    let parent = neighbors.(2) in
    let parentq, parentv = parentinfo.(parent + 1) in

    let v = Qtr.apply parentq curv in
    let v = Vec.add v parentv in

    let q = Qtr.compose curq parentq in
    parentinfo.(i + 1) <- (q, v);
    xyzofvec parentv, xyzofvec v;
  in
  Array.mapi mapf bones;
;;

let vertices1 bones rotations poseno =
  let parentinfo = Array.make (Array.length bones + 1) (Qtr.id, Vec.origin) in
  let mapf i (pos, (name, offsets, floats, neighbors, float)) =
    let curq = rotations.(i).(poseno) in
    let curv = vof floats in

    let parent = neighbors.(2) in
    let parentq, parentv = parentinfo.(parent + 1) in

    let v = Qtr.apply parentq curv in
    let v = Vec.add v parentv in

    let q = Qtr.compose curq parentq in
    parentinfo.(i + 1) <- (q, v);
    xyzofvec parentv, xyzofvec v;
  in
  Array.mapi mapf bones;
;;

let draw data =
  let rta = vertices data in

  let sphere origin =
    GlMat.mode `modelview;
    GlMat.push ();
    GlMat.translate3 (origin);
    let radius = Rend.view.Rend.radial_scale*.0.009 in
    GluQuadric.sphere ~radius ~stacks:5 ~slices:5 ();
    GlMat.pop ()
  in

  fun () ->
    GlDraw.polygon_mode `both `line;
    Gl.disable  `depth_test;
    Array.iteri
      (fun i (pos, (n, d, m, h, f)) ->
        GlDraw.line_width 0.1;
        GlDraw.color (0., 0., 1.);
        let v0, v1 = rta.(i) in
        sphere v1;
        GlDraw.line_width 2.0;
        GlDraw.begins `lines;
        GlDraw.vertex3 v0;
        GlDraw.vertex3 v1;
        GlDraw.ends ();
      ) data;
    Gl.enable `depth_test;
;;

let draw1 bones =
  fun rotations poseno ->
    let rta = vertices1 bones rotations poseno in
    fun () ->
      GlDraw.polygon_mode `both `line;
      GlDraw.color (0., 0., 1.);
      Gl.disable  `depth_test;
      for i = 0 to Array.length bones - 1 do
        (* GlDraw.line_width 0.1; *)
        let v0, v1 = rta.(i) in
        (* sphere v1; *)
        GlDraw.line_width 2.0;
        GlDraw.begins `lines;
        GlDraw.vertex3 v0;
        GlDraw.vertex3 v1;
        GlDraw.ends ();
      done;
      Gl.enable `depth_test;
;;

let func bones anim =
  let posecount, rotations = anim in
  let draw0 = draw bones in
  let draw1 = draw1 bones in
  let rec subfunc dodraw poseno draw = function
    | Rend.Char ('f'|'b' as c) ->
        let poseno =
          if c = 'b' then
            let poseno = if poseno = 0 then posecount else poseno in
            (poseno - 1) mod posecount
          else
            (poseno + 1) mod posecount
        in
        let draw = draw1 rotations poseno in
        Anb.skin rotations poseno;
        Skin.anim ();
        Rend.Func (subfunc dodraw poseno draw)
    | Rend.Char 'r' ->
        Rend.Func (subfunc dodraw 0 draw0)
    | Rend.Draw ->
        if dodraw then draw ();
        Rend.Func (subfunc dodraw poseno draw)
    | Rend.Char 's' -> Rend.Func (subfunc (not dodraw) poseno draw)
    | Rend.Char _ -> Rend.Func (subfunc dodraw poseno draw)
  in
  subfunc false 0 (draw bones)
;;

let dummy draw =
  let rec subfunc = function
    | Rend.Draw -> draw (); Rend.Func subfunc
    | _ -> Rend.Func subfunc
  in
  subfunc
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
      let name = Filename.chop_extension name ^ ".skb" in
      let xff, sbuf = Xff.test2 name in
      let bones = r1 xff sbuf in
      begin try
          let anim_name =
            match !Rend.anb_name with
            | None -> raise (Failure "no animation set")
            | Some name ->name
          in
          let xff, sbuf = Xff.test2 (Filename.basename anim_name) in
          let anim = Anb.r xff sbuf in
          skin bones;
          func bones anim
        with exn ->
          prerr_endline (Printexc.to_string exn);
          dummy (draw bones)
      end;
    with exn ->
      prerr_endline (Printexc.to_string exn);
      dummy (fun () -> ())
  in
  Rend.add_func func
;;