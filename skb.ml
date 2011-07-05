open Format;;

let skels = ref [||];;
let qof f = Qtr.make f.(5) f.(6) f.(7) f.(8);;
let vof f = Vec.make f.(1) f.(2) f.(3);;

type t = int * (string * int32 array * float array * int array * float);;

let rt sbuf strtab =
  let d = Array.init 3 (fun n -> Xff.r32 sbuf (n*4)) in
  let f9 = Array.init 9 (fun n -> Xff.rfloat sbuf (12 +n*4)) in
  let h = Array.init 3 (fun n -> Xff.rint sbuf (48 + n*4)) in
  let f = Xff.rfloat sbuf 60 in
  let name = Xff.rcstrtabent strtab 0 (Int32.to_int d.(0)) in
  if !Rend.dump_bones
  then
    print_endline name
  ;
  (name, d, f9, h, f)
;;

let r1 xff sbufxff =
  if Array.length xff.Xff.sections != 2
  then
    Xff.sbuferr sbufxff 0 "number of xff sections is not 2"
  ;
  let sectpos = xff.Xff.sections.(1).Xff.off in
  let sectbuf = Xff.sbufplus sbufxff sectpos in
  let skbbuf = Xff.sbufplus sectbuf xff.Xff.entry in
  if not (Xff.check32 skbbuf 0 1l)
  then
    Xff.sbuferr skbbuf 0 "bad skb signature"
  ;

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

let vertices bones quats rootq rootv =
  let parentinfo = Array.make (Array.length bones + 1) (Qtr.id, Vec.origin) in
  parentinfo.(0) <- rootq, rootv;
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

let draw ?(dsphere=false) rta =
  fun () ->
    GlDraw.polygon_mode `both `line;
    GlDraw.color (0., 0., 1.);
    Gl.disable  `depth_test;
    for i = 0 to Array.length rta - 1 do
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

let animate skin quats =
  Skin.set_anim skin quats;
  Skin.anim skin;
;;

let rec obj skin bones anim rootqv =
  let posecount, rotations = anim in
  let clip poseno =
    let poseno' = poseno mod posecount in
    if poseno' < 0 then posecount + poseno' else poseno'
  in
  let skbquats = skbquats bones in

  let roots =
    Array.init (Array.length bones + 1) (fun i -> Qtr.id, Vec.origin)
  in
  let updateroots rootq rootv quats =
    roots.(0) <- rootq, rootv;
    let iter i (pos, (name, offsets, floats, neighbors, float)) =
      let curq = quats.(i) in
      let curv = vof floats in

      let parent = neighbors.(2) in
      let parentq, parentv = roots.(parent + 1) in

      let v = Qtr.apply parentq curv in
      let v = Vec.add v parentv in

      let q = Qtr.compose curq parentq in
      roots.(i+1) <- (q, v);
    in
    Array.iteri iter bones;
  in
  let skeldraw n =
    let rta =
      let mapf i (pos, (name, offsets, floats, neighbors, float)) =
        let parent = neighbors.(2) in
        let _, parentv = roots.(parent + 1) in
        let _, v = roots.(i + 1) in
        Vec.elts parentv, Vec.elts v
      in
      Array.mapi mapf bones
    in
    match n mod 3 with
    | 0 -> ()
    | 1 -> draw rta ()
    | _ -> draw ~dsphere:true rta ()
  in
  let draw = object (self)
    val drawindex = 0
    val sposeno = 0
    val dposeno = clip 1
    val quats = skbquats
    val t = 0.0
    val dir = 1
    val mutable nodes = []

    method private advance nodes quats dir =
      let t = t +. !Rend.slerp_step in
      let q, v = rootqv () in
      updateroots q v quats;
      if t >= 1.0
      then
        let sposeno = dposeno
        and dposeno = clip (dposeno + dir) in
        {< quats = quats; sposeno = sposeno; dposeno = dposeno; t = 0.0;
           nodes = nodes >}
      else
        {< quats = quats; sposeno = sposeno; dposeno = dposeno; t = t;
           nodes  = nodes >}

    method help =
      [("s", "toggle skeleton (S type)",
       if drawindex = 0 then "off" else string_of_int drawindex)
      ;"B", sprintf "toggle animation direction", string_of_int dir
      ;"f", "forward one frame", sprintf "%d, %f" sposeno t
      ;"b", "backward one frame", sprintf "%d, %f" sposeno t
      ;"", "", "total frames " ^ string_of_int posecount
      ;"r", "go to bind pose", ""
      ;"1,2", "go to first/last frame", ""
      ]

    method draw =
      List.iter (fun o -> o#draw) nodes;
      skeldraw drawindex

    method char c =
      match c with
      | 'n' | 'f' | 'b' ->
          let quats = Anb.interpolated rotations sposeno dposeno t in
          animate skin quats;
          let nodes = List.map (fun o -> o#char c) nodes in
          self#advance nodes quats
            (match c with
            | 'n' -> dir
            | 'f' -> 1
            | _ -> -1)

      | 'B' ->
          let nodes = List.map (fun o -> o#char c) nodes in
          {< dir = -dir; nodes = nodes >}

      | 'r' ->
          Skin.set_anim skin skbquats;
          Skin.anim skin;
          let nodes = List.map (fun o -> o#char c) nodes in
          {< drawindex = 1; quats = skbquats; nodes = nodes >}

      | '1' | '2' ->
          let sposeno, dposeno =
            if c = '1'
            then 0, clip 1
            else clip (posecount - 1), 0
          in
          let quats = Anb.exact rotations sposeno in
          animate skin quats;
          let t = if dir > 0 then 1.0 else 0.0 in
          let nodes = List.map (fun o -> o#char c) nodes in
          {< quats = quats; sposeno = sposeno; dposeno = dposeno; t = t;
             nodes = nodes >}

      | 's' ->
          let nodes = List.map (fun o -> o#char c) nodes in
          {< drawindex = if drawindex = 0 then 1 else 0; nodes = nodes >}
      | 'S' ->
          let nodes = List.map (fun o -> o#char c) nodes in
          {< drawindex = (drawindex + 1) mod 3; nodes = nodes >}
      | _ -> self

    method private getqv i () = roots.(i+1)

    method addat bone_name skin' bones' anim' =
      let rec find i =
        if i = Array.length bones
        then
          let rec find = function
            | [] -> None
            | c :: cs ->
                match c#addat bone_name skin' bones' anim' with
                | None -> find cs
                | some -> some
          in
          find nodes
        else
          let (_, (bone_name', _, _, _, _)) = bones.(i) in
          if bone_name' <> bone_name
          then find (i+1)
          else
            let node = obj skin' bones' anim' (self#getqv i) in
            Skin.set_parent skin' skin i;
            Some {< nodes = node :: nodes >}
      in
      find 0
  end
  in
  draw
;;

let doskin skin bones =
  let skel = Array.map
    (fun (_, (_, _, floats, neighbors, _)) -> (neighbors.(2), floats))
    bones
  in
  Skin.set_skel skin skel;
;;

let drawobj =
  (object (self)
    val index = 0
    method help =
      !skels.(index)#help

    method draw = ()

    method char c =
      Array.iteri (fun i skel -> !skels.(i) <- skel#char c) !skels;
      self
  end)
;;

let hack () =
  Rend.add_obj
    (object (self)
      val index = 0
      method help = []

      method draw =
        Array.iter (fun skel -> skel#draw) !skels

      method char c = self
    end)
;;

let regbones name skin model bones anim =
  let noroot () =
    let obj = obj skin bones anim (fun () -> Qtr.id, Vec.origin) in
    if !skels = [||] then Rend.add_obj drawobj;
    skels := Array.append !skels [|obj|];
  in
  match model.Rend.root with
  | None ->
      noroot ()

  | Some name ->
      let rec loop i =
        if i = Array.length !skels
        then noroot ()
        else
          match !skels.(i)#addat name skin bones anim with
          | None -> loop (i+1)
          | Some obj -> !skels.(i) <- obj
      in loop 0
;;

let main skin name model =
  try
    let xff, sbuf = Xff.test2 name in
    let bones = r1 xff sbuf in
    begin match model.Rend.anims with
    | [] ->
        let quats = skbquats bones in
        doskin skin bones;
        let anim = 1, Array.map (fun q -> [|q|]) quats in
        regbones name skin model bones anim

    | list ->
        let ranb aname =
          let xff, sbuf = Xff.test2 (Filename.basename aname) in
          let anim = Anb.r xff sbuf in
          let (_, abones) = anim in
          if Array.length abones != Array.length bones
          then
            failwith (sprintf "invalid animation %s for skeleton %s"
                         aname name);
          anim
        in
        let rec run1 = function
          | [] -> failwith "no animations"
          | hd :: tl ->
              try
                let anim = ranb hd in
                run2 anim tl
              with exn ->
                prerr_endline (Printexc.to_string exn);
                run1 tl
        and run2 accu = function
          | [] -> accu
          | hd :: tl ->
              let accu =
                try
                  Anb.append accu (ranb hd)
                with exn ->
                  prerr_endline (Printexc.to_string exn);
                  accu
              in
              run2 accu tl
        in
        let anim = run1 list in
        doskin skin bones;
        regbones name skin model bones anim
    end;
  with exn ->
    prerr_endline (Printexc.to_string exn);
;;
